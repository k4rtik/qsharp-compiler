//------------------------------------------------------------------------------
// This code was generated by a tool.
// <auto-generated />
//------------------------------------------------------------------------------
//
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <vector>

#include "CLI11.hpp"

#include "QirContext.hpp"
#include "QirRuntime.hpp"
#include "SimFactory.hpp"

using namespace Microsoft::Quantum;
using namespace std;
    

// This is the function corresponding to the QIR entry-point.
extern "C" void UseResultArg( // NOLINT
    char ResultArgInteropValue
);


const char InteropResultZeroAsChar = 0x0;
const char InteropResultOneAsChar = 0x1;
map<string, char> ResultAsCharMap{
    {"0", InteropResultZeroAsChar},
    {"Zero", InteropResultZeroAsChar},
    {"1", InteropResultOneAsChar},
    {"One", InteropResultOneAsChar}
};

int main(int argc, char* argv[])
{
    CLI::App app("QIR Standalone Entry Point Inputs Reference");

    // Initialize simulator.
    unique_ptr<IRuntimeDriver> sim = CreateFullstateSimulator();
    QirContextScope qirctx(sim.get(), false /*trackAllocatedObjects*/);

    // Add the --simulation-output options.
    // N.B. This option should be present in all standalone drivers.
    string simulationOutputFile;
    CLI::Option* simulationOutputFileOpt = app.add_option(
        "--simulation-output", simulationOutputFile,
        "File where the output produced during the simulation is written");

    char ResultArgCliValue;
    ResultArgCliValue = InteropResultZeroAsChar;
    app.add_option("--ResultArg", ResultArgCliValue, "A Result value for the ResultArg argument")->required()
        ->transform(CLI::CheckedTransformer(ResultAsCharMap, CLI::ignore_case));

    // With all the options added, parse arguments from the command line.
    CLI11_PARSE(app, argc, argv);

    char ResultArgInteropValue = ResultArgCliValue;

    // Redirect the simulator output from std::cout if the --simulation-output option is present.
    ostream* simulatorOutputStream = &cout;
    ofstream simulationOutputFileStream;
    if (!simulationOutputFileOpt->empty())
    {
        simulationOutputFileStream.open(simulationOutputFile);
        SetOutputStream(simulationOutputFileStream);
        simulatorOutputStream = &simulationOutputFileStream;
    }

    // Run simulation and write the output of the operation to the corresponding stream.
    UseResultArg(
        ResultArgInteropValue
    );


    simulatorOutputStream->flush();
    if (simulationOutputFileStream.is_open())
    {
        simulationOutputFileStream.close();
    }
}
