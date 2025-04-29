#include "TriggersTab.h"

#include <QLabel>

TriggersTab::TriggersTab(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);
    formLayout = new QFormLayout();

    observableComboBox = new QComboBox(this);
    connect(observableComboBox, &QComboBox::currentTextChanged,
            this, &TriggersTab::onObservableChanged);
    formLayout->addRow("Observable:", observableComboBox);

    sampleLimitCheckBox = new QCheckBox("Sample Limit > 500", this);
    qtaBoundsCheckBox = new QCheckBox("QTA Bound Violation", this);
    failureRateCheckBox = new QCheckBox("Failure Rate < 0.95", this);

    formLayout->addRow(sampleLimitCheckBox);
    formLayout->addRow(qtaBoundsCheckBox);
    formLayout->addRow(failureRateCheckBox);

    connect(sampleLimitCheckBox, &QCheckBox::stateChanged, this, &TriggersTab::onTriggerChanged);
    connect(qtaBoundsCheckBox, &QCheckBox::stateChanged, this, &TriggersTab::onTriggerChanged);
    connect(failureRateCheckBox, &QCheckBox::stateChanged, this, &TriggersTab::onTriggerChanged);

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
    delete formLayout;
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
        auto manager = triggerManagerForCurrentObservable();

        manager.clearAllTriggers();

        if (sampleLimitCheckBox->isChecked()) {
            manager.addTrigger(
                TriggerType::SampleLimit,
                TriggerDefs::Conditions::SampleLimit(sampleLimitThreshold),
                [this](const DeltaQ& dq, const QTA&) {
                    addTriggeredMessage("Sample limit exceeded");
                }
            );
        }

        if (qtaBoundsCheckBox->isChecked()) {
            manager.addTrigger(
                TriggerType::QTAViolation,
                TriggerDefs::Conditions::QTABounds(),
                [this](const DeltaQ&, const QTA&) {
                    addTriggeredMessage("QTA bounds violated");
                }
            );
        }

        if (failureRateCheckBox->isChecked()) {
            auto system = Application::getInstance().getSystem();


            std::string name = observableComboBox->currentText().toStdString();
            auto observable = system->getObservable(name);
            manager.addTrigger(
                TriggerType::Failure,
                TriggerDefs::Conditions::FailureRate(observable->getQTA().cdfMax),
                [this](const DeltaQ&, const QTA&) {
                    addTriggeredMessage("Failure rate below threshold");
                }
            );
        }
    }
    catch (std::exception& e) {
        return;
    }
}

void TriggersTab::updateCheckboxStates() {
    // You can query from the manager to update actual state (optional)
    sampleLimitCheckBox->setChecked(false);
    qtaBoundsCheckBox->setChecked(false);
    failureRateCheckBox->setChecked(false);
}

TriggerManager TriggersTab::triggerManagerForCurrentObservable() {
    auto system = Application::getInstance().getSystem();

    if (!system) throw std::runtime_error("System does not exist");

    std::string name = observableComboBox->currentText().toStdString();
    auto observable = system->getObservable(name);
    if (!observable) throw std::runtime_error("Observable does not exist");

    return observable->getTriggerManager(); // Must be implemented in Observable
}

void TriggersTab::addTriggeredMessage(const QString& msg) {
    triggeredList->addItem(msg);
}

void TriggersTab::onTriggeredItemClicked(QListWidgetItem* item) {
    // Future: emit a signal or open a plot window
    QString msg = item->text();
    qDebug() << "Item clicked:" << msg;
}
