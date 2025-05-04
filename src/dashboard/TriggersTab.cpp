#include "TriggersTab.h"

#include <QLabel>
#include <unordered_set>

TriggersTab::TriggersTab(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);
    formLayout = new QFormLayout();

    observableComboBox = new QComboBox(this);
    connect(observableComboBox, &QComboBox::currentTextChanged,
            this, &TriggersTab::onObservableChanged);
    formLayout->addRow("Observable:", observableComboBox);

    sampleLimitCheckBox = new QCheckBox("Sample Limit >", this);
    sampleLimitSpinBox = new QSpinBox(this);
    sampleLimitSpinBox->setRange(1, 100000); // or whatever range makes sense
    sampleLimitSpinBox->setValue(sampleLimitThreshold); // initialize to default value

    sampleLimitLayout = new QHBoxLayout();
    sampleLimitLayout->addWidget(sampleLimitCheckBox);
    sampleLimitLayout->addWidget(sampleLimitSpinBox);

    sampleLimitWidget = new QWidget(this);
    sampleLimitWidget->setLayout(sampleLimitLayout);

    formLayout->addRow(sampleLimitWidget);


    qtaBoundsCheckBox = new QCheckBox("QTA Bound Violation", this);
    failureRateCheckBox = new QCheckBox("Failure Rate < 0.95", this);

    formLayout->addRow(qtaBoundsCheckBox);
    formLayout->addRow(failureRateCheckBox);

    connect(sampleLimitCheckBox, &QCheckBox::checkStateChanged, this, &TriggersTab::onTriggerChanged);
    connect(sampleLimitSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &TriggersTab::onTriggerChanged);

    connect(qtaBoundsCheckBox, &QCheckBox::checkStateChanged, this, &TriggersTab::onTriggerChanged);
    connect(failureRateCheckBox, &QCheckBox::checkStateChanged, this, &TriggersTab::onTriggerChanged);

    mainLayout->addLayout(formLayout);

    // Triggered list view
    triggeredList = new QListWidget(this);
    triggeredList->setSelectionMode(QAbstractItemView::SingleSelection);
    triggeredList->setMinimumHeight(150);
    connect(triggeredList, &QListWidget::itemClicked,
            this, &TriggersTab::onTriggeredItemClicked);

    mainLayout->addWidget(new QLabel("Triggered Events:"));
    mainLayout->addWidget(triggeredList);

    Application::getInstance().addObserver([this]() { this->populateObservables(); });

    populateObservables();
}

TriggersTab::~TriggersTab() {

    delete mainLayout;
}

void TriggersTab::populateObservables() {
    try {
        auto system = Application::getInstance().getSystem();
        observableComboBox->clear();
        for (const auto& [name, _] : system->getProbes())
            observableComboBox->addItem(QString::fromStdString(name));
        for (const auto& [name, _] : system->getOutcomes())
            observableComboBox->addItem(QString::fromStdString(name));
    }
    catch (std::exception& e) {
        return;
    }
}

void TriggersTab::onObservableChanged(const QString&) {
    updateCheckboxStates();
}
void TriggersTab::onTriggerChanged() {
    try {
        auto observable = getCurrentObservable();

        // Sample Limit Trigger
        if (sampleLimitCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::SampleLimit,
                TriggerDefs::Conditions::SampleLimit(sampleLimitSpinBox->value()),
                [this](const DeltaQ&, const QTA&) {
                    addTriggeredMessage("Sample limit exceeded");
                },
                true,
                sampleLimitSpinBox->value()
            );
        } else {
            observable->removeTrigger(TriggerType::SampleLimit);
        }

        // QTA Bounds Trigger
        if (qtaBoundsCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::QTAViolation,
                TriggerDefs::Conditions::QTABounds(),
                [this](const DeltaQ&, const QTA&) {
                    addTriggeredMessage("QTA bounds violated");
                },
                true,
                std::nullopt
            );
        } else {
            observable->removeTrigger(TriggerType::QTAViolation);
        }

        // Failure Rate Trigger
        if (failureRateCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::Failure,
                TriggerDefs::Conditions::FailureRate(observable->getQTA().cdfMax),
                [this](const DeltaQ&, const QTA&) {
                    addTriggeredMessage("Failure rate below threshold");
                },
                true,
                std::nullopt
            );
        } else {
            observable->removeTrigger(TriggerType::Failure);
        }
    }
    catch (std::exception& e) {
        return;
    }
}

void TriggersTab::updateCheckboxStates() {
    try {
        auto observable = getCurrentObservable();
        auto& manager = observable->getTriggerManager();

        auto all = manager.getAllTriggers();
        std::unordered_set<TriggerType> activeTypes;
        for (const auto& trigger : all) {
            if (trigger.enabled)
                activeTypes.insert(trigger.type);
        }

        sampleLimitCheckBox->setChecked(activeTypes.count(TriggerType::SampleLimit));
        qtaBoundsCheckBox->setChecked(activeTypes.count(TriggerType::QTAViolation));
        failureRateCheckBox->setChecked(activeTypes.count(TriggerType::Failure));

        for (const auto& t : all) {
            if (t.type == TriggerType::SampleLimit) {
                if (auto limit = t.sampleLimitValue) {
                    sampleLimitSpinBox->setValue(*limit);
                }
            }
        }
    } catch (std::exception &e) {
        sampleLimitCheckBox->setChecked(false);
        qtaBoundsCheckBox->setChecked(false);
        failureRateCheckBox->setChecked(false);
    }
}



std::shared_ptr<Observable> TriggersTab::getCurrentObservable() {
    auto system = Application::getInstance().getSystem();

    if (!system) throw std::runtime_error("System does not exist");

    std::string name = observableComboBox->currentText().toStdString();
    auto observable = system->getObservable(name);
    if (!observable) throw std::runtime_error("Observable does not exist");

    return observable;
}

void TriggersTab::addTriggeredMessage(const QString& msg) {
    triggeredList->addItem(msg);
}

void TriggersTab::onTriggeredItemClicked(QListWidgetItem* item) {
    // Future: emit a signal or open a plot window
    QString msg = item->text();
    qDebug() << "Item clicked:" << msg;
}
