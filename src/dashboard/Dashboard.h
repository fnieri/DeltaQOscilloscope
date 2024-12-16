//
// Created by francy on 04/12/24.
//

#ifndef DASHBOARD_H
#define DASHBOARD_H

#include "JsonComponent.h"
#include <QApplication>
#include <QComboBox>
#include <QDialog>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMainWindow>
#include <QPushButton>
#include <QScrollArea>
#include <QTimer>
#include <QTreeWidget>
#include <QVBoxLayout>
#include <QVector>
#include <qpushbutton.h>
#include <qwidget.h>
class Dashboard : public QMainWindow
{
    Q_OBJECT
private:
    QWidget *centralWidget;
    QComboBox *operatorChoice;

    QWidget *nameWidget;
    QLabel *nameLabel;
    QLineEdit *nameLineEdit;

    QWidget *startEventWidget;
    QLabel *startEventLabel;
    QLineEdit *startEventLineEdit;

    QWidget *endEventWidget;
    QLabel *endEventLabel;
    QLineEdit *endEventLineEdit;

    QPushButton *addButton;

    QTreeWidget *componentTree;

    std::vector<JsonComponent> componentsJson; // Store components as JSON

    void addComboBox();
    void setUpLineEdits();
    void setupAddButton();
    void setUpComponentTree();
    void saveToFile();
    bool areFieldsValid(const std::string &name);
private slots:
    void onAdd();
    void onEditItem(QTreeWidgetItem *item, int column);

public:
    Dashboard(QWidget *parent = 0);
    ~Dashboard();
};

#endif // DASHBOARD_H
