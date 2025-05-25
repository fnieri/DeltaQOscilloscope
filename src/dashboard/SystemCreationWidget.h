#ifndef SYSTEMCREATIONWIDGET_H
#define SYSTEMCREATIONWIDGET_H

#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWidget>

/**
 * @class SystemCreationWidget
 * @brief A QWidget that allows users to create, edit, load, and save system definitions.
 */
class SystemCreationWidget : public QWidget
{
    Q_OBJECT

public:
    explicit SystemCreationWidget(QWidget *parent = nullptr);

    /**
     * @brief Retrieves the current system text from the editor.
     * @return The system text as a std::string.
     */
    std::string getSystemText() const;

    /**
     * @brief Sets the system text in the editor.
     * @param text The new system definition text.
     */
    void setSystemText(const std::string &text);

Q_SIGNALS:
    /**
     * @brief Emitted when the system is successfully updated.
     */
    void systemUpdated();

    /**
     * @brief Emitted when the system is successfully saved.
     */
    void systemSaved();

    /**
     * @brief Emitted when a system is successfully loaded.
     */
    void systemLoaded();

private Q_SLOTS:
    /**
     * @brief Parses the text and updates the system instance.
     */
    void onUpdateSystem();

    /**
     * @brief Saves the current system text to a file.
     */
    void saveSystemTo();

    /**
     * @brief Loads a system from a file and updates the editor.
     */
    void loadSystem();

private:
    QTextEdit *systemTextEdit;          ///< Editor widget for system text.
    QPushButton *updateSystemButton;    ///< Button to update system.
    QPushButton *saveSystemButton;      ///< Button to save system.
    QPushButton *loadSystemButton;      ///< Button to load system.
    QLabel *systemLabel;                ///< Label describing the editor.

    QVBoxLayout *mainLayout;            ///< Layout for the main components.
    QHBoxLayout *buttonLayout;          ///< Layout for the buttons.
};

#endif // SYSTEMCREATIONWIDGET_H
