//
// Created by francy on 04/12/24.
//

#ifndef DASHBOARD_H
#define DASHBOARD_H

#include <QApplication>
#include <QDialog>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QHBoxLayout>
#include <QLabel>
#include <QMainWindow>
#include <QPushButton>
#include <QScrollArea>
#include <QTimer>
#include <QVBoxLayout>
#include <QVector>

class Dashboard : public QMainWindow
{
    Q_OBJECT

public:
    Dashboard(QWidget *parent = 0);
    ~Dashboard();
};

#endif // DASHBOARD_H
