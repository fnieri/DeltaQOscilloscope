#pragma once

#include "ConfidenceInterval.h"
#include "DeltaQ.h"
#include "DeltaQRepr.h"
#include <map>
#include <optional>

/**
 * @class Snapshot
 * @brief Represents a snapshot of observed and calculated DeltaQ values along with QTAs
 */
class Snapshot
{
    std::string observableName; ///< Name of the observable being tracked.
    std::map<uint64_t, DeltaQRepr> observedDeltaQs; ///< Map of observed DeltaQ values at times t.
    std::map<uint64_t, DeltaQRepr> calculatedDeltaQs; ///< Map of calculated DeltaQ values at times t
    std::map<uint64_t, QTA> QTAs; ///< Map of QTAs at time t

public:
    /// @brief Default constructor.
    Snapshot() = default;

    // --- Observed DeltaQ Methods ---
    /**
     * @brief Adds an observed DeltaQ to the snapshot.
     * @param timestamp The lower time bound at which the DeltaQ was observed.
     * @param deltaQ The DeltaQ value to store.
     * @param bounds Confidence interval bounds for the DeltaQ.
     */
    void addObservedDeltaQ(std::uint64_t timestamp, const DeltaQ &deltaQ, const std::vector<Bound> &bounds);

    /**
     * @brief Retrieves the oldest observed DeltaQ.
     * @return The oldest DeltaQRepr (based on timestamp order).
     */
    DeltaQ getOldestObservedDeltaQ() const;

    /// @brief Removes the oldest observed DeltaQ entry (FIFO order).
    void removeOldestObservedDeltaQ();

    // --- Calculated DeltaQ Methods ---
    /**
     * @brief Adds a calculated DeltaQ to the snapshot.
     * @param timestamp The lower time bound at which the DeltaQ was calculated.
     * @param deltaQ The DeltaQ value to store.
     * @param bounds Confidence interval bounds for the DeltaQ.
     */
    void addCalculatedDeltaQ(std::uint64_t timestamp, const DeltaQ &deltaQ, const std::vector<Bound> &bounds);

    /**
     * @brief Retrieves the oldest calculated DeltaQ.
     * @return The oldest DeltaQRepr (based on timestamp order).
     */
    DeltaQ getOldestCalculatedDeltaQ() const;

    /// @brief Removes the oldest calculated DeltaQ entry (FIFO order).
    void removeOldestCalculatedDeltaQ();

    // --- QTA Methods ---
    /**
     * @brief Adds a QTA to the snapshot.
     * @param timestamp The time associated with the QTA.
     * @param qta The QTA value to store.
     */
    void addQTA(std::uint64_t timestamp, const QTA &qta);

    // --- Size Management ---
    /// @return The number of observed DeltaQs stored.
    std::size_t getObservedSize() const;

    /// @return The number of calculated DeltaQs stored.
    std::size_t getCalculatedSize() const;

    /**
     * @brief Truncates observed/calculated DeltaQs to a specified size (removes oldest entries).
     * @param size The maximum number of entries to retain.
     */
    void resizeTo(size_t size);

    // --- Name Management ---
    /**
     * @brief Sets the name of the observable.
     * @param name New name for the observable.
     */
    void setName(const std::string &name);

    /// @return The current observable name.
    std::string getName() &;

    // --- Data Retrieval ---
    /// @return A vector of all observed DeltaQReprs (sorted by timestamp).
    std::vector<DeltaQRepr> getObservedDeltaQs() const &;

    /// @return A vector of all calculated DeltaQReprs (sorted by timestamp).
    std::vector<DeltaQRepr> getCalculatedDeltaQs() const &;

    /**
     * @brief Retrieves an observed DeltaQ at a specific timestamp.
     * @param timestamp The time to query.
     * @return The DeltaQRepr if found, or `std::nullopt` otherwise.
     */
    std::optional<DeltaQRepr> getObservedDeltaQAtTime(std::uint64_t timestamp);

    /**
     * @brief Retrieves a calculated DeltaQ at a specific timestamp.
     * @param timestamp The time to query.
     * @return The DeltaQRepr if found, or `std::nullopt` otherwise.
     */
    std::optional<DeltaQRepr> getCalculatedDeltaQAtTime(std::uint64_t timestamp);

    /// @return A vector of all QTAs (sorted by timestamp).
    std::vector<QTA> getQTAs() const &;
};
