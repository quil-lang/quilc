#include <iostream>
#include <tweedledum/gates/mcmt_gate.hpp>
#include <tweedledum/io/write_unicode.hpp>
#include <tweedledum/io/quil.hpp>
#include <tweedledum/io/qasm.hpp>
#include <tweedledum/networks/netlist.hpp>
#include <tweedledum/networks/gg_network.hpp>
#include <tweedledum/algorithms/synthesis/dbs.hpp>
#include <tweedledum/algorithms/synthesis/diagonal_synth.hpp>
#include <tweedledum/algorithms/synthesis/stg.hpp>

// This is a wrapper to tweedledum that exposes to C a very small
// subset of tweedledum's functionality. This can be compiled to a
// shared library and then used via CFFI.

extern "C" {

  extern char* tweedledum_synthesis_diagonal(double* angles, uint32_t size) {
    using namespace tweedledum;
    std::vector<double> vangles(angles, angles + size);
    std::cout << vangles.size() << std::endl;
    auto network = diagonal_synth<gg_network<mcmt_gate>>(vangles);

    std::stringstream ss;
    write_quil(network, ss);
    const std::string resultstr = ss.str();
    char* str = (char*) malloc( (resultstr.length()+1) * sizeof(char) );
    strcpy(str, resultstr.c_str());
    return str;
  }

  extern char* tweedledum_synthesis_dbs(uint32_t* perm, uint32_t size) {
    using namespace tweedledum;
    std::vector<uint32_t> permutation(perm, perm+size);
    auto network = dbs<netlist<mcmt_gate>>(permutation, stg_from_spectrum());
    
    std::stringstream ss;
    write_quil(network, ss);
    const std::string resultstr = ss.str();
    char* str = (char*) malloc( (resultstr.length()+1) * sizeof(char) );
    strcpy(str, resultstr.c_str());
    return str;
  }

}
