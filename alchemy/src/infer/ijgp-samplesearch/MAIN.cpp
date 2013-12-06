
#include "MAIN.h"

#include <iostream>
#include <fstream>
#include "BE.h"
#include "GM.h"
#include "Graph.h"
#include "RB_IJGP_Sampler.h"
#include "SampleSearch.h"
#include "Solver.h"
#include "Parameters.h"
#include "VEC.h"

namespace ss{

string getOutFilename(string& str)
{
	int d = -1;
	for (int i = str.size() - 1; i > -1; i--) {
		if (str.data()[i] == '/') {
			d = i;
			break;
		}
	}
	return str.substr(d + 1, str.size());
}
int help()
{
	cerr << "Usage: ijgp-samplesearch [options] <uaifilename> <evidfilename> <time-bound> <task>\n";
	cerr << "-------------------------------------------------------------------------------------\n";
	cerr << "\t The four arguments are required\n";
	cerr << "\t <task> can be PR or MAR\n";
	cerr << "\t The output will be stored in <uaifilename>.task in the working directory\n";
	cerr << "\t [Working directory is the directory in which the code is run]\n";
	cerr << "-------------------------------------------------------------------------------------\n";
	cerr << " \t The following three options help you control the parameters\n";
	cerr << " \t If these options are not used, then SampleSearch selects one for you based on the\n";
	cerr << " \t properties of the problem instance at hand\n";
	cerr << " \t -i [int] : The i-bound of IJGP \n";
	cerr << " \t -w [int] : The w-cutset bound \n";
	cerr << " \t -n [int] : Number of iterations for IJGP\n";
	exit(1);
}
string uaifilename;
int seed;
string evidfilename;
string task;
string outfilename;
vector<vector<pair<int, int> > > evidences;
int UAI2010Parameters::I_BOUND_=-1;
int UAI2010Parameters::RB_BOUND_=-1;
int UAI2010Parameters::NUM_ITERATIONS_=-1;
clock_t total_time = 100;//atoi(argv[3]);
void readParameters(int argc, const char* argv[])
{
	if (argc < 4) {
		help();
	}
	for(int i=1;i<argc-5;i++){
		string mystring=argv[i];
		if (mystring == "-i"){
			UAI2010Parameters::I_BOUND_=atoi(argv[++i]);
		}
		else if (mystring == "-w"){
			UAI2010Parameters::RB_BOUND_=atoi(argv[++i]);
		}
		else if (mystring == "-n"){
			UAI2010Parameters::NUM_ITERATIONS_=atoi(argv[++i]);
		}
		else{
			cerr<<"ERROR ------ Wrong options\n";
			help();
			exit(-1);
		}
	}
	uaifilename = argv[argc - 4];
	evidfilename = argv[argc - 3];
	//seed = atoi(argv[argc - 2]);
	seed = time(NULL);
	total_time=atoi(argv[argc-2]);
	task = argv[argc - 1];
	outfilename = getOutFilename(uaifilename) + "." + task;
	cout << outfilename << endl;
	ofstream out(outfilename.c_str());
	out << task << endl;
	out.close();
}
void readEvidence()
{

	ifstream in(evidfilename.c_str());
	int num_evidence = 0;
	if (in.good()) {
		in >> num_evidence;
		evidences = vector<vector<pair<int, int> > > (num_evidence);
		for (int i = 0; i < num_evidence; i++) {
			int curr_num_evidence;
			in >> curr_num_evidence;
			for (int j = 0; j < curr_num_evidence; j++) {
				int var, val;
				in >> var >> val;
				//gm.variables[var]->value() = val;
				evidences[i].push_back(pair<int, int> (var, val));
			}
		}
	}
	cout << "Evidence read\n";
	in.close();
}
void readEvidence(GM& gm, vector<int>& evidence)
{

	ifstream in(evidfilename.c_str());

	if (in.good()) {
		int num_evidence;
		in >> num_evidence;
		evidence = vector<int> (num_evidence);
		for (int i = 0; i < num_evidence; i++) {
			int var, val;
			in >> var >> val;
			gm.variables[var]->value() = val;
			evidence[i] = var;
		}
	}
	cout << "Evidence read\n";
	in.close();
}

int MAIN(int argc, const char* argv[])
{
	Timer timer;
	timer.start();

	cerr << "Time-bound=" << total_time << endl;
	double total_memory = 1;
	total_memory *= 1E107;
	if (total_time < 50) {
		total_time = (int) (((double) total_time - 2) * (double) 0.93);
	}
	else {
		total_time = (int) (((double) total_time - 2) * (double) 0.97);
	}
	readParameters(argc, argv);
	GM gm;
	gm.readUAI08(uaifilename.c_str());

	// Read Evidence
	vector<int> evidence;
	readEvidence(gm, evidence);
	total_time -= timer.elapsed_seconds();
	cout << "Total time = " << total_time << endl;

	ofstream out(outfilename.c_str(), ios::app);
	if (task == "PR") {
		gm.removeIrrelevantNetwork(evidence);
	}
	else if (task == "MAR") {
		gm.setEvidenceBeliefsUAI08(evidence);
	}
	if (gm.mode == DET) {
		gm.reduceDomains();
	}
	// SampleSearch is very expensive. Therefore if the SAT instance is empty, set gm mode to non-deterministic
	if (gm.mode == DET) {
		gm.convertToSATUAI10();
		if (gm.instance.iClauseCount() == 0) {
			cerr << "SampleSearch not necessary\n";
			gm.mode = POSITIVE;
		}
	}
	UAI2010Parameters params(gm, total_time, task);
	cerr << "# sampled = " << params.s_order.size() << endl;
	if (params.exact_inf_test()) {
		if (task == "PR") {
			total_time -= timer.elapsed_seconds();
			if (gm.mode == DET) {
				VEC_DET(gm, params.bw_order, params.s_order, total_time, out);
			}
			else {
				VEC(gm, params.bw_order, params.s_order, total_time, out);
			}
			out.close();
			return 0;
		}
		else {
			total_time -= timer.elapsed_seconds();
			if (gm.mode == DET) {
				VEC_MAR_DET(gm, params.bw_order, params.s_order, params.bw_clusters, total_time, out);
			}
			else {
				VEC_MAR(gm, params.bw_order, params.s_order, params.bw_clusters, total_time, out);
			}
			out.close();
			return 0;
		}
	}
	JG jg(gm, params.i_bound, params.num_iterations, params.bt_order, LSS);
	cerr << "Join graph constructed\n";
	jg.propagate();
	//jg.propagateDFS();

	total_time -= timer.elapsed_seconds();

	int p_bound = 0;
	if (gm.mode == DET) {
		RBSampleSearch ss(out);
		ss.interval = 100;
		if (task == "PR") {
			cerr << "Running SampleSarch\n";
			ss.computePEApp(gm, jg, p_bound, params.bw_order, total_time, params.s_order, params.max_restarts);
		}
		else if (task == "MAR") {
			cerr << "Running SampleSarch\n";
			ss.computeBeliefsApp_Improved(gm, jg, p_bound, params.bw_order, total_time, params.s_order, params.max_restarts, params.bw_clusters);
			/*
			 if (gm.type == BAYES) {
			 ss.computeBayesBeliefsApp(gm, other_gm, jg, p_bound, order,
			 curr_time, sampling_order, max_restarts);
			 } else {
			 ss.computeBeliefsApp(gm, jg, p_bound, order, curr_time,
			 sampling_order, max_restarts);
			 }
			 */
		}
	}
	else {
		if (task == "PR") {
			RB_IJGP_Sampler sampler;
			out << sampler.computePE(gm, jg, p_bound, params.bw_order, params.s_order, total_time) << endl;
		}
		else if (task == "MAR") {
			RB_IJGP_Sampler sampler;
			sampler.computeBeliefs_Improved(gm, jg, p_bound, params.bw_order, params.bw_clusters, params.s_order, total_time, out);
		}
	}
	out.close();
}
}
