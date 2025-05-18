#include "CustomLegendEntry.h"
#include <QHBoxLayout>
CustomLegendEntry::CustomLegendEntry(const QString &name, const QColor &color, QWidget *parent)
    : QWidget(parent)
{
    layout = new QHBoxLayout(this);
    colorBox = new QLabel;
    colorBox->setFixedSize(12, 12);
    colorBox->setStyleSheet(QString("background-color: %1").arg(color.name()));
    nameLabel = new QLabel(name);
    layout->addWidget(colorBox);
    layout->addWidget(nameLabel);
    layout->addStretch();
}
