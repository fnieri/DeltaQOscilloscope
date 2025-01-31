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
#include <qevent.h>
#include <qpushbutton.h>
#include <qwidget.h>
class Dashboard : public QMainWindow
{
    Q_OBJECT
private:
    QWidget *centralWidget;
    QComboBox *operatorChoice;

    // Components used to add a component
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

    std::vector<JsonComponent> componentsJson;

    // For file saving and keeping track of modifications
    QString currentFileName;
    bool isModified = false;

    void addComboBox();
    void setUpLineEdits();
    void setupAddButton();
    void setUpComponentTree();
    bool areFieldsValid(const std::string &name);
    void updateWindowTitle();
    void closeEvent(QCloseEvent *event) override;
    void saveToFile();
    void loadFromFile();
    void addComponentToTree(const JsonComponent &component);
    void addAllCategoriesToTree();
private slots:
    void onAdd();
    void onEditItem(QTreeWidgetItem *item, int column);

public:
    Dashboard(QWidget *parent = 0);
    ~Dashboard();
};

#endif // DASHBOARD_H
