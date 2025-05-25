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
    // Main layout setup
    mainLayout = new QVBoxLayout(this);
    formLayout = new QFormLayout();

    // Observable selection dropdown
    observableComboBox = new QComboBox(this);
    connect(observableComboBox, &QComboBox::currentTextChanged, this, &TriggersTab::onObservableChanged);
    formLayout->addRow("Probe:", observableComboBox);

    // Sample limit controls
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

    // QTA bounds violation checkbox
    qtaBoundsCheckBox = new QCheckBox("QTA Bound Violation", this);
    formLayout->addRow(qtaBoundsCheckBox);

    // Connect signals
    connect(sampleLimitCheckBox, &QCheckBox::checkStateChanged, this, &TriggersTab::onTriggerChanged);
    connect(sampleLimitSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &TriggersTab::onTriggerChanged);
    connect(qtaBoundsCheckBox, &QCheckBox::checkStateChanged, this, &TriggersTab::onTriggerChanged);

    mainLayout->addLayout(formLayout);

    // Triggered events list
    triggeredList = new QListWidget(this);
    triggeredList->setSelectionMode(QAbstractItemView::SingleSelection);
    triggeredList->setMinimumHeight(150);
    connect(triggeredList, &QListWidget::itemClicked, this, &TriggersTab::onTriggeredItemClicked);

    mainLayout->addWidget(new QLabel("Triggered Snapshots:"));
    mainLayout->addWidget(triggeredList);

    // Set up system observer
    Application::getInstance().addObserver([this]() { this->populateObservables(); });

    populateObservables();
}

TriggersTab::~TriggersTab()
{
    delete mainLayout;
}

/**
 * @brief Populates the observable dropdown with available probes and outcomes.
 */
void TriggersTab::populateObservables()
{
    try {
        auto system = Application::getInstance().getSystem();
        observableComboBox->clear();
        for (const auto &[name, obs] : system->getObservables()) {
            if (obs) {
                observableComboBox->addItem(QString::fromStdString(name));
            }
        }
    } catch (std::exception &) {
        return;
    }
}

/**
 * @brief Handles observable selection changes.
 * @param name The newly selected observable name (unused, signal requires parameter).
 */
void TriggersTab::onObservableChanged(const QString &)
{
    updateCheckboxStates();
}

/**
 * @brief Updates triggers when conditions change.
 */
void TriggersTab::onTriggerChanged()
{
    try {
        auto observable = getCurrentObservable();
        std::string name = observable->getName();

        // Update sample limit trigger
        if (sampleLimitCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::SampleLimit,
                TriggerDefs::Conditions::SampleLimit(sampleLimitSpinBox->value()),
                [this, name](const DeltaQ &, const QTA &, std::uint64_t time) {
                    this->captureSnapshots(time, name);
                },
                true,
                sampleLimitSpinBox->value());
        } else {
            observable->removeTrigger(TriggerType::SampleLimit);
        }

        // Update QTA bounds trigger
        if (qtaBoundsCheckBox->isChecked()) {
            observable->addTrigger(
                TriggerType::QTAViolation,
                TriggerDefs::Conditions::QTABounds(),
                [this, name](const DeltaQ &, const QTA &, std::uint64_t time) {
                    this->captureSnapshots(time, name);
                },
                true,
                std::nullopt);
        } else {
            observable->removeTrigger(TriggerType::QTAViolation);
        }
    } catch (std::exception &) {
        return;
    }
}

/**
 * @brief Captures snapshots when triggers are activated.
 * @param time The timestamp of the trigger event.
 * @param name The name of the observable that triggered.
 */
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

                    // Format timestamp for display
                    qint64 msTime = time / 1000000;
                    QDateTime timestamp = QDateTime::fromMSecsSinceEpoch(msTime);

                    // Create display string
                    QString timestampStr = timestamp.toString();
                    std::ostringstream oss;
                    oss << "Snapshot at: " << timestampStr.toStdString() << " from " << name;
                    QString snapshotString = QString::fromStdString(oss.str());

                    // Add to triggered list
                    auto *item = new QListWidgetItem(snapshotString);
                    item->setData(Qt::UserRole, static_cast<qulonglong>(time));
                    triggeredList->addItem(item);
                    timer->deleteLater();

                    system->setRecording(false);
                });

                timer->start(5000);
            },
            Qt::QueuedConnection);
    }
}

/**
 * @brief Updates checkbox states based on current triggers.
 */
void TriggersTab::updateCheckboxStates()
{
    try {
        auto observable = getCurrentObservable();
        auto &manager = observable->getTriggerManager();

        // Get active trigger types
        auto all = manager.getAllTriggers();
        std::unordered_set<TriggerType> activeTypes;
        for (const auto &trigger : all) {
            if (trigger.enabled)
                activeTypes.insert(trigger.type);
        }

        // Update UI to match active triggers
        sampleLimitCheckBox->setChecked(activeTypes.count(TriggerType::SampleLimit));
        qtaBoundsCheckBox->setChecked(activeTypes.count(TriggerType::QTAViolation));

        // Update sample limit value if trigger exists
        for (const auto &t : all) {
            if (t.type == TriggerType::SampleLimit && t.sampleLimitValue) {
                sampleLimitSpinBox->setValue(*t.sampleLimitValue);
            }
        }
    } catch (std::exception &) {
        sampleLimitCheckBox->setChecked(false);
        qtaBoundsCheckBox->setChecked(false);
    }
}

/**
 * @brief Gets the currently selected observable.
 * @return current observable.
 * @throws std::runtime_error if system or observable doesn't exist.
 */
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

/**
 * @brief Handles triggered item clicks to show snapshots.
 * @param item The clicked list item containing snapshot data.
 */
void TriggersTab::onTriggeredItemClicked(QListWidgetItem *item)
{
    if (!item)
        return;

    // Retrieve timestamp from item data
    qulonglong timestamp = item->data(Qt::UserRole).toULongLong();
    if (timestamp == 0)
        return;

    // Find and display corresponding snapshot
    auto system = Application::getInstance().getSystem();
    const auto &snapshots = system->getAllSnapshots();
    auto it = snapshots.find(timestamp);
    if (it != snapshots.end()) {
        const auto &snapshotList = it->second;

        auto *viewer = new SnapshotViewerWindow(const_cast<std::vector<Snapshot> &>(snapshotList));
        viewer->setAttribute(Qt::WA_DeleteOnClose);
        viewer->resize(800, 600);
        viewer->show();
    }
}
