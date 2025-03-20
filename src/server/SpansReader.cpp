#include "SpansReader.h"
#include "../diagram/Sample.h"
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <nlohmann/json.hpp>
#include <sstream>
#include <thread>

namespace fs = std::filesystem;
using json = nlohmann::json;
void processSpans(const std::string &line)
{
    try {
        if (line.empty())
            return;

        json j = json::parse(line);
        std::vector<Sample> samples;

        for (const auto &resourceSpan : j["resourceSpans"]) {
            for (const auto &scopeSpan : resourceSpan["scopeSpans"]) {
                for (const auto &span : scopeSpan["spans"]) {
                    samples.emplace_back(Sample {.componentName = span["name"].get<std::string>(),
                        .startTime = std::stod(span["startTimeUnixNano"].get<std::string>()) / 1e9,
                        .endTime = std::stod(span["endTimeUnixNano"].get<std::string>()) / 1e9});
                }
            }
        }

        // Process samples here
        for (const auto &sample : samples) {
            std::cout << "Processed: " << sample.componentName << " (" << std::fixed << std::setprecision(12) << sample.startTime << " - " << sample.endTime
                      << ")\n";
        }
    } catch (const json::exception &e) {
        std::cerr << "JSON error: " << e.what() << "\n";
    }
}

void monitorFile(const std::string &filePath)
{
    fs::path path(filePath);
    std::streampos lastPos = 0;
    std::uintmax_t lastSize = 0;
    auto lastWriteTime = fs::last_write_time(path);

    while (true) {
        try {
            if (!fs::exists(path)) {
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                continue;
            }

            const auto currentWriteTime = fs::last_write_time(path);
            const auto currentSize = fs::file_size(path);

            // Handle file rotation or truncation
            if (currentWriteTime != lastWriteTime || currentSize < lastSize) {
                lastPos = 0;
                lastSize = currentSize;
                lastWriteTime = currentWriteTime;
            }

            if (currentSize > lastPos) {
                std::ifstream file(filePath, std::ios::binary);
                file.seekg(lastPos);

                std::string buffer;
                buffer.resize(currentSize - lastPos);
                file.read(&buffer[0], buffer.size());
                lastPos = file.tellg();

                std::istringstream iss(buffer);
                std::string line;
                while (std::getline(iss, line)) {
                    processSpans(line);
                }
            }
        } catch (const fs::filesystem_error &e) {
            std::cerr << "Filesystem error: " << e.what() << "\n";
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
}
