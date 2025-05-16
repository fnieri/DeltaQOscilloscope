#ifndef QTAINPUTWIDGET_H
#define QTAINPUTWIDGET_H

#include <QWidget>
#include <QLineEdit>
#include <QComboBox>
#include <QFormLayout>
#include <QLabel>
#include <QPushButton>

class QTAInputWidget : public QWidget
{
    Q_OBJECT

public:
    explicit QTAInputWidget(QWidget *parent = nullptr);

    double getPerc25() const;
    double getPerc50() const;
    double getPerc75() const;
    double getCdfMax() const;
    QString getSelectedObservable() const;

public Q_SLOTS:
    void populateComboBox();    // Load available observables
    void loadObservableSettings(); // Load values from system when observable is selected
    void onSaveButtonClicked();
private:
    QComboBox *observableComboBox;
    QLineEdit *perc25Edit;
    QLineEdit *perc50Edit;
    QLineEdit *perc75Edit;
    QLineEdit *cdfMaxEdit;
    QPushButton *saveButton;
    QLabel *qtaLabel;
};

#endif // QTAINPUTWIDGET_H
