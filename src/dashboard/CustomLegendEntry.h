

#pragma once

#include <QLabel>
#include <QWidget>
#include <qboxlayout.h>

class CustomLegendEntry : public QWidget
{
    Q_OBJECT
    QHBoxLayout *layout;
    QLabel *colorBox;
    QLabel *nameLabel;

public:
    CustomLegendEntry(const QString &name, const QColor &color, QWidget *parent = nullptr);
};
