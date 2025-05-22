#ifndef SYSTEMCREATIONWIDGET_H
#define SYSTEMCREATIONWIDGET_H

#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWidget>
#include <qboxlayout.h>

class SystemCreationWidget : public QWidget
{
    Q_OBJECT

public:
    explicit SystemCreationWidget(QWidget *parent = nullptr);

    std::string getSystemText() const;
    void setSystemText(const std::string &text);

Q_SIGNALS:
    void systemUpdated();
    void systemSaved();
    void systemLoaded();

private Q_SLOTS:
    void onUpdateSystem();
    void saveSystemTo();
    void loadSystem();

private:
    QTextEdit *systemTextEdit;
    QPushButton *updateSystemButton;
    QPushButton *saveSystemButton;
    QPushButton *loadSystemButton;
    QLabel *systemLabel;

    QVBoxLayout *mainLayout;
    QHBoxLayout *buttonLayout;
};

#endif // SYSTEMCREATIONWIDGET_H
