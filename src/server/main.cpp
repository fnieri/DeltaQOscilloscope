#include "SpansReader.h"
#include <thread>
int main()
{
    std::string filePath = "spans.json";
    std::thread monitorThread(monitorFile, filePath);

    monitorThread.join();
    return 0;
}
