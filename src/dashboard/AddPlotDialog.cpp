
#include "AddPlotDialog.h"
#include <QDialogButtonBox>
#include <QLabel>
#include <QListWidgetItem>
#include <QVBoxLayout>

AddPlotDialog::AddPlotDialog(std::shared_ptr<System> system, const QStringList &preselectedItems, QWidget *parent)
    : QDialog(parent)
{
    setWindowTitle("Select Outcomes and Probes");

    listWidget = new QListWidget(this);
    probesWidget = new QListWidget(this);

    QVBoxLayout *layout = new QVBoxLayout(this);
    // Outcomes
    QLabel *outcomeLabel = new QLabel("Outcomes:", this);
    layout->addWidget(outcomeLabel);
    for (const auto &[name, outcome] : system->getOutcomes()) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(name), listWidget);
        item->setSelected(preselectedItems.contains(item->text()));
    }

    // Probes
    QLabel *probesLabel = new QLabel("Probes:", this);
    layout->addWidget(probesLabel);
    for (const auto &[name, probe] : system->getProbes()) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(name), probesWidget);
        item->setSelected(preselectedItems.contains(item->text()));
    }

    QLabel *operationsLabel = new QLabel("Operations:", this);
    layout->addWidget(operationsLabel);

    operationsWidget = new QListWidget(this);
    operationsWidget->setSelectionMode(QAbstractItemView::SingleSelection);

    QStringList operations = {"Convolution", "All-to-Finish", "First-to-Finish", "Probabilistic Choice"};
    for (const QString &op : operations) {
        operationsWidget->addItem(op);
    }

    listWidget->setSelectionMode(QAbstractItemView::MultiSelection);
    probesWidget->setSelectionMode(QAbstractItemView::MultiSelection);

    layout->addWidget(outcomeLabel);
    layout->addWidget(listWidget);
    layout->addWidget(probesLabel);
    layout->addWidget(probesWidget);
    layout->addWidget(operationsLabel);
    layout->addWidget(operationsWidget);

    // OK & Cancel Buttons
    QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttonBox);

    connect(buttonBox, &QDialogButtonBox::accepted, this, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, this, &QDialog::reject);
}

QStringList AddPlotDialog::getSelectedItems() const
{
    QStringList selected;
    for (QListWidgetItem *item : listWidget->selectedItems()) {
        selected.append(item->text());
    }
    for (QListWidgetItem *item : probesWidget->selectedItems()) {
        selected.append(item->text());
    }
    return selected;
}

SelectionResult AddPlotDialog::getSelections() const
{
    SelectionResult result;

    for (QListWidgetItem *item : listWidget->selectedItems()) {
        result.selectedOutcomes.append(item->text());
    }
    for (QListWidgetItem *item : probesWidget->selectedItems()) {
        result.selectedProbes.append(item->text());
    }
    if (operationsWidget->selectedItems().size() > 0) {
        result.selectedOperation = operationsWidget->selectedItems().first()->text();
    }

    return result;
}
