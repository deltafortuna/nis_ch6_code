#include <algorithm>
#include <string>
#include <iterator>
#include <bitset>
#include <random>
#include <cmath>
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <sstream>
#include <regex>

using namespace std;

#include "params.h"
#include "params.cc" // inclusion causes parameter values to be read
#include "allele.h"
#include "individual.h"
#include "population.h"
#include "metapopulation.h"

int main(int argc, char *argv[]) {
	mt19937 engine(time(0));  //initialize the random engine
	Population::e  = engine;
	mt19937 engine2(time(0));
	Metapopulation::f = engine2;
	Population pop;

	cout << popsize << " is popsize" << endl;
	cout << "MIGRATION matrix:" << endl;
	mig.print_matrix();

	Metapopulation meta;

	// simulate for runlength generations
	for (int i =0; i < runlength; ++i) {
		meta.reproduce_and_migrate(i);
		if (i % 25 == 0) { cout << "gen " << i << endl;}
	}
	meta.close_output_files();
	return 0;
}

// static variables
mt19937 Population::e;
mt19937 Metapopulation::f;
