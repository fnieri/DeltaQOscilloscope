
#include "TriggersTab.h"

#include "SnapshotViewerWindow.h"
#include <QDateTime>
#include <QLabel>
#include <QString>
#include <QTimer>
#include <cstdlib>
#include <sstream>
#include <string>
#include <unordered_set>
TriggersTab::TriggersTab(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);
    formLayout = new QFormLayout();

    observableComboBox = new QComboBox(this);
    connect(observableComboBox, &QComboBox::currentTextChanged, this, &TriggersTab::onObservableChanged);
    formLayout->addRow("Observable:", observableComboBox);

    sampleLimitCheckBox = new QCheckBox("Sample Limit >", this);
    sampleLimitSpinBox = new QSpinBox(this);
    sampleLimitSpinBox->setRange(1, 100000);
    sampleLimitSpinBox->setValue(sampleLimitThreshold);

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

    triggeredList = new QListWidget(this);
    triggeredList->setSelectionMode(QAbstractItemView::SingleSelection);
    triggeredList->setMinimumHeight(150);
    connect(triggeredList, &QListWidget::itemClicked, this, &TriggersTab::onTriggeredItemClicked);

    mainLayout->addWidget(new QLabel("Triggered Snapshots:"));
    mainLayout->addWidget(triggeredList);

    Application::getInstance().addObserver([this]() { this->populateObservables(); });

    populateObservables();
}

TriggersTab::~TriggersTab()
{
    delete mainLayout;
}

void TriggersTab::populateObservables()
{
    try {
        auto system = Application::getInstance().getSystem();
        observableComboBox->clear();
        for (const auto &[name, _] : system->getProbes())
            observableComboBox->addItem(QString::fromStdString(name));
        for (const auto &[name, _] : system->getOutcomes())
            observableComboBox->addItem(QString::fromStdString(name));
    } catch (std::exception &) {
        return;
    }
}

void TriggersTab::onObservableChanged(const QString &)
{
    updateCheckboxStates();
}

void TriggersTab::onTriggerChanged()
{
    try {
        auto observable = getCurrentObservable();
        std::string name = observable->getName();
        if (sampleLimitCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::SampleLimit, TriggerDefs::Conditions::SampleLimit(sampleLimitSpinBox->value()),
                [this, name](const DeltaQ &, const QTA &, std::uint64_t time) { this->captureSnapshots(time, name); }, true, sampleLimitSpinBox->value());
        } else {
            observable->removeTrigger(TriggerType::SampleLimit);
        }

        if (qtaBoundsCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::QTAViolation, TriggerDefs::Conditions::QTABounds(),
                [this, name](const DeltaQ &, const QTA &, std::uint64_t time) { this->captureSnapshots(time, name); }, true, std::nullopt);
        } else {
            observable->removeTrigger(TriggerType::QTAViolation);
        }

        if (failureRateCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::Failure, TriggerDefs::Conditions::FailureRate(observable->getQTA().cdfMax),
                [this, name](const DeltaQ &, const QTA &, std::uint64_t time) { this->captureSnapshots(time, name); }, true, std::nullopt);
        } else {
            observable->removeTrigger(TriggerType::Failure);
        }
    } catch (std::exception &) {
        return;
    }
}

void TriggersTab::captureSnapshots(std::uint64_t time, const std::string &name)
{
    auto system = Application::getInstance().getSystem();
    if (!system->isRecording()) {
        system->setRecording(true);
        QMetaObject::invokeMethod(
            this,
            [this, name, system, time]() {
                auto *timer = new QTimer(this);
                timer->setSingleShot(true);

                connect(timer, &QTimer::timeout, this, [=]() {
                    system->getObservablesSnapshotAt(time);

                    qint64 msTime = time / 1000000;
                    QDateTime timestamp = QDateTime::fromMSecsSinceEpoch(msTime);

                    QString timestampStr = timestamp.toString();
                    std::ostringstream oss;
                    oss << "Snapshot at: " << timestampStr.toStdString() << " from " << name;
                    QString snapshotString = QString::fromStdString(oss.str());

                    auto *item = new QListWidgetItem(snapshotString);
                    item->setData(Qt::UserRole, static_cast<qulonglong>(time)); // Store raw timestamp
                    triggeredList->addItem(item);
                    timer->deleteLater();

                    system->setRecording(false);
                });

                timer->start(5000);
            },
            Qt::QueuedConnection);
    }
}

void TriggersTab::updateCheckboxStates()
{
    try {
        auto observable = getCurrentObservable();
        auto &manager = observable->getTriggerManager();

        auto all = manager.getAllTriggers();
        std::unordered_set<TriggerType> activeTypes;
        for (const auto &trigger : all) {
            if (trigger.enabled)
                activeTypes.insert(trigger.type);
        }

        sampleLimitCheckBox->setChecked(activeTypes.count(TriggerType::SampleLimit));
        qtaBoundsCheckBox->setChecked(activeTypes.count(TriggerType::QTAViolation));
        failureRateCheckBox->setChecked(activeTypes.count(TriggerType::Failure));

        for (const auto &t : all) {
            if (t.type == TriggerType::SampleLimit && t.sampleLimitValue) {
                sampleLimitSpinBox->setValue(*t.sampleLimitValue);
            }
        }
    } catch (std::exception &) {
        sampleLimitCheckBox->setChecked(false);
        qtaBoundsCheckBox->setChecked(false);
        failureRateCheckBox->setChecked(false);
    }
}

std::shared_ptr<Observable> TriggersTab::getCurrentObservable()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        throw std::runtime_error("System does not exist");

    std::string name = observableComboBox->currentText().toStdString();
    auto observable = system->getObservable(name);
    if (!observable)
        throw std::runtime_error("Observable does not exist");

    return observable;
}
void TriggersTab::onTriggeredItemClicked(QListWidgetItem *item)
{
    if (!item)
        return;

    // Retrieve raw timestamp
    qulonglong timestamp = item->data(Qt::UserRole).toULongLong();
    if (timestamp == 0)
        return;

    auto system = Application::getInstance().getSystem();
    const auto &snapshots = system->getAllSnapshots();
    auto it = snapshots.find(timestamp);
    if (it != snapshots.end()) {
        const auto &snapshotList = it->second;
        qDebug() << "Snapshot at" << timestamp << "contains" << snapshotList.size() << "entries";

        auto *viewer = new SnapshotViewerWindow(const_cast<std::vector<Snapshot> &>(snapshotList));
        viewer->setAttribute(Qt::WA_DeleteOnClose);
        viewer->resize(800, 600);
        viewer->show();
    }
}
