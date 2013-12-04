#include "ClauseList.h"
#include "Clause.h"
#include "SATInstance.h"
#include "BigNum.h"
#include "SATSolver.h"
#include "SampleSearch.h"
#include "Solver.h"
#include "BE.h"
#include "JT.h"
#include <zlib.h>
#include "ORStore.h"

char satfile[100] = "temp.cnf";
void setupSolver(Solver& S, GM& gm, bool reduce = false)
{
	gm.convertToSATUAI10();
	reduce = false;
	if (reduce) {
		gm.instance.vSortClausesByLength();
		SATSolver xSatSolver(&gm.instance);
		xSatSolver.bPreprocess(3, 10);
		xSatSolver.vIncorporateLearnedClauses();
		gm.instance.vSortClausesByLength();
	}
	for (int i = 0; i < gm.instance.iVariableCount; i++) {
		S.newVar();
	}
	for (int i = 0; i < gm.instance.iClauseCount(); i++) {
		vec<Lit> lits;
		RClause* tmp_clause = gm.instance.pClause(i);
		for (int j = 0; j < tmp_clause->iVariableCount(); j++) {
			int var = tmp_clause->eConstrainedVariable(j);
			if (tmp_clause->iIsNegated(j)) {
				lits.push(~Lit(var));
			}
			else {
				lits.push(Lit(var));
			}
		}
		S.addClause(lits);
	}
}
string getDateAndTime()
{
	time_t p = time(NULL);
	char *s = ctime(&p);
	s[strlen(s) - 1] = 0;
	string out(s);
	return out;
}
string printbytes(unsigned int bytes)
{

	char tmp[100];
	double gb = -5.0;
	double mb;
	if (bytes > 1073741824) {
		gb = (double) bytes / (double) 1073741824;
	}

	mb = (double) bytes / (double) 1048576;

	if (gb > 0) {
		sprintf(tmp, "%fGB", gb);

	}
	else {
		sprintf(tmp, "%fMB", mb);
	}
	string tmp1 = tmp;
	return tmp1;
}
double checkNumSolutions(GM& gm, vec<Lit>& assumptions)
{
	ofstream out("sat_tmp.cnf");
	out << "p cnf " << gm.variables.size() << " " << gm.clauses.size() + assumptions.size() << endl;
	for (int i = 0; i < gm.clauses.size(); i++) {
		for (int j = 0; j < gm.clauses[i].size(); j++) {
			int Var = var(gm.clauses[i][j]);
			int p = (Lit(Var) == gm.clauses[i][j]) ? (Var + 1) : (-(Var + 1));
			out << p << " ";
		}
		out << 0 << endl;
	}
	for (int i = 0; i < assumptions.size(); i++) {
		int Var = var(assumptions[i]);
		int p = (Lit(Var) == assumptions[i]) ? (Var + 1) : (-(Var + 1));
		out << p << " 0\n";
	}
	out.close();
	system("/home/vibhav/work/cachet/cachet sat_tmp.cnf -q > t");
	ifstream in("t");
	double k;
	in >> k;
	//system(" cat t");
	in.close();
	if (k <= 0.0) {
		exit(1);
	}
	return k;

}
bool normalize_table(vector<Double>& table)
{
	Double norm_const;
	for (int i = 0; i < table.size(); i++) {
		norm_const += table[i];
	}
	for (int i = 0; i < table.size(); i++) {
		table[i] /= norm_const;
	}
}
int generateSample(vector<Double>& table, myRandom& random, bool normalize = true)
{
	if (normalize) {
		normalize_table(table);
	}
	int value = INVALID_VALUE;
	double rand_num = random.getDouble();
	double cdf = 0.0;
	double curr_var_weight = 0.0;
	for (int k = 0; k < table.size(); k++) {
		if (!table[k].isZero()) {
			value = k;
			cdf += table[k].value();
			curr_var_weight = table[k].value();
			if (rand_num <= cdf) {
				value = k;
				break;
			}
		}
	}
	return value;
}
bool isConsistent(int var, int value, vec<Lit>& assumptions, ORStore& store, Solver& solver)
{
	if (store.check(var, value, false) == INCONSISTENT) {
		return false;
	}
	if (store.check(var, value, false) == CONSISTENT) {
		store.AddAssignment(var, value);
		return true;
	}
	assert(store.check(var,value,false)==DONTKNOW);
	if (solver.solve(assumptions)) {
		store.AddAssignment(var, value);
		return true;
	}
	store.AddNoGood(var, value);
	return false;
}
bool isConsistentSAT(int var, int value, vec<Lit>& assumptions, ORStore& store, Solver& solver)
{
	int other_value = (value == 0) ? (1) : (0);
	if (store.check(var, value, false) == INCONSISTENT) {
		assert(store.check(var,other_value,false)==CONSISTENT);
		return false;
	}
	if (store.check(var, value, false) == CONSISTENT) {
		store.AddAssignment(var, value);
		return true;
	}
	assert(store.check(var,value,false)==DONTKNOW);
	if (solver.solve(50, assumptions) == l_True) {
		store.AddAssignment(var, value);
		return true;
	}
	store.AddNoGood(var, value);
	store.AddGood(var, other_value);
	return false;
}
//=================================================================================================
// DIMACS Parser:

#define CHUNK_LIMIT 1048576

class StreamBuffer
{
	gzFile in;
	char buf[CHUNK_LIMIT];
	int pos;
	int size;

	void assureLookahead()
	{
		if (pos >= size) {
			pos = 0;
			size = gzread(in, buf, sizeof(buf));
		}
	}

public:
	StreamBuffer(gzFile i) :
		in(i), pos(0), size(0)
	{
		assureLookahead();
	}

	int operator *()
	{
		return (pos >= size) ? EOF : buf[pos];
	}
	void operator ++()
	{
		pos++;
		assureLookahead();
	}
};

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

template<class B>
static void skipWhitespace(B& in)
{
	while ((*in >= 9 && *in <= 13) || *in == 32)
		++in;
}

template<class B>
static void skipLine(B& in)
{
	for (;;) {
		if (*in == EOF || *in == '\0')
			return;
		if (*in == '\n') {
			++in;
			return;
		}
		++in;
	}
}

template<class B>
static int parseInt(B& in)
{
	int val = 0;
	bool neg = false;
	skipWhitespace(in);
	if (*in == '-')
		neg = true, ++in;
	else if (*in == '+')
		++in;
	if (*in < '0' || *in > '9')
		reportf("PARSE ERROR! Unexpected char: %c\n", *in), exit(3);
	while (*in >= '0' && *in <= '9')
		val = val * 10 + (*in - '0'), ++in;
	return neg ? -val : val;
}

template<class B>
static void readClause(B& in, Solver& S, vec<Lit>& lits)
{
	int parsed_lit, var;
	lits.clear();
	for (;;) {
		parsed_lit = parseInt(in);
		if (parsed_lit == 0)
			break;
		var = abs(parsed_lit) - 1;
		while (var >= S.nVars())
			S.newVar();
		lits.push((parsed_lit > 0) ? Lit(var) : ~Lit(var));
	}
}

template<class B>
static bool match(B& in, char* str)
{
	for (; *str != 0; ++str, ++in)
		if (*str != *in)
			return false;
	return true;
}

template<class B>
static void parse_DIMACS_main(B& in, Solver& S)
{
	vec<Lit> lits;
	for (;;) {
		skipWhitespace(in);
		if (*in == EOF)
			break;
		else if (*in == 'p') {
			if (match(in, "p cnf")) {
				int vars = parseInt(in);
				int clauses = parseInt(in);
				//reportf("|  Number of variables:  %-12d                                         |\n", vars);
				//reportf("|  Number of clauses:    %-12d                                         |\n", clauses);
			}
			else {
				reportf("PARSE ERROR! Unexpected char: %c\n", *in), exit(3);
			}
		}
		else if (*in == 'c' || *in == 'p')
			skipLine(in);
		else
			readClause(in, S, lits), S.addClause(lits);
	}
}

// Inserts problem into solver.
//
static void parse_DIMACS(gzFile input_stream, Solver& S)
{
	StreamBuffer in(input_stream);
	parse_DIMACS_main(in, S);
}

//=================================================================================================


SampleSearch::SampleSearch(GM& gm_, JG& jg_, int p_bound, vector<int>& order_) :
	gm(gm_), jg(jg_)
{
	helper = IJGPSamplingHelper(&gm, &jg, 0, order_);
	order = vector<int> (order_.size());
	for (int i = 0; i < order.size(); i++)
		order[order.size() - i - 1] = order_[i];
	gm.convertToSAT();

	/*if(strcmp(satfile,"temp.cnf")==0)
	 {
	 SATInstance xInstance;
	 SATSolver xSATSolver(&xInstance);
	 xInstance.bReadDimacs("temp.cnf");
	 xSATSolver.bPreprocess(3,10);
	 ofstream xNewInstance;
	 xNewInstance.open("temp.cnf", ios::out);
	 xInstance.vOutputDimacs(xNewInstance);
	 }*/

}

void SampleSearch::computeBeliefs(int time_limit, int num_samples, vector<vector<Double> >& beliefs)
{
	Double weight;
	myRandom random;
	time_t start_time, curr_time;
	start_time = time(NULL);
	Solver solver;
	setupSolver(solver, gm);
	beliefs = vector<vector<Double> > (gm.variables.size());
	for (int i = 0; i < gm.variables.size(); i++) {
		beliefs[i] = vector<Double> (gm.variables[i]->domain_size());
	}

	for (int samples = 0; samples != num_samples; samples++) {

		solver.sampler = this;
		curr_time = time(NULL);
		if (num_samples == INVALID_VALUE && (curr_time - start_time) >= time_limit) {
			break;
		}
		// Initialize
		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		bool ret_value = solver.solve_sample();
		assert(ret_value);
		cerr << "Sample " << samples << " generated \n";
		Double curr_weight = getWeight(solver.model);
		for (int i = 0; i < gm.variables.size(); i++) {
			assert(gm.variables[i]->value()!=INVALID_VALUE);
			beliefs[i][gm.variables[i]->value()] += curr_weight;
		}
		weight += curr_weight;
		cerr << "\tSample " << samples << " weight computed\n";
	}

	for (int i = 0; i < gm.variables.size(); i++) {
		gm.variables[i]->value() = INVALID_VALUE;
		for (int j = 0; j < beliefs[i].size(); j++)
			beliefs[i][j] /= weight;
	}

}

Double SampleSearch::computePE(int time_limit, int num_samples)
{
	Double weight;
	myRandom random;
	time_t start_time, curr_time;
	start_time = time(NULL);
	Solver solver;
	setupSolver(solver, gm);

	for (int samples = 0; samples != num_samples; samples++) {

		solver.sampler = this;
		curr_time = time(NULL);
		if (num_samples == INVALID_VALUE && (curr_time - start_time) >= time_limit) {
			return weight / Double((double) samples);
		}
		// Initialize
		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		bool ret_value = solver.solve_sample();
		assert(ret_value);
		cerr << "Sample " << samples << " generated \n";
		Double curr_weight = getWeight(solver.model);
		for (int i = 0; i < gm.variables.size(); i++) {
			assert(gm.variables[i]->value()!=INVALID_VALUE);

		}

		weight += curr_weight;
		cerr << "\tSample " << samples << " weight computed\n";
	}
	return weight / Double((double) num_samples);
}
void SampleSearch::getSample(vector<Double>& bf)
{
	// The following should be changed....very bad hack


	Solver solver;
	gzFile in = gzopen(satfile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	// End...change this

	solver.sampler = this;
	bool ret_value = solver.solve_sample();
	assert(ret_value);
	getWeight(solver.model, bf);
}
int SampleSearch::getSample(int& variable, bool& value, vec<char>& assignment)
{

	int csp_variable = INVALID_VALUE;
	// Select the earliest unassigned variable

	for (int i = 0; i < gm.variables.size(); i++) {
		int var = order[i];

		for (int j = 0; j < gm.csp_to_sat_variables[var].size(); j++) {
			if (toLbool(assignment[gm.csp_to_sat_variables[var][j]]) == l_Undef) {
				csp_variable = var;
				//cerr<<var<<endl;
				break;
			}
		}
		if (csp_variable == var)
			break;

	}

	// If all varables are assigned a value return
	if (csp_variable == INVALID_VALUE) {
		variable = -1;
		value = false;
		return 1;
	}
	gm.variables[csp_variable]->value() = INVALID_VALUE;
	Double cdf, norm_const;
	vector<Double> func_table(gm.variables[csp_variable]->domain_size());
	//Set values of all variables in the function
	// Assign values to all variables of the function

	//SF* function=&helper.sampling_functions[csp_variable];
	Function* function = &helper.getFunction(csp_variable);

	for (int i = 0; i < function->variables().size(); i++) {
		if (function->variables()[i]->id() == csp_variable)
			continue;
		function->variables()[i]->value() = INVALID_VALUE;
		int var = function->variables()[i]->id();
		bool val_set = false;
		//function->[i]->resetCurrDomain();

		for (int j = 0; j < gm.csp_to_sat_variables[var].size(); j++) {
			if (toLbool(assignment[gm.csp_to_sat_variables[var][j]]) == l_True) {
				val_set = true;
				gm.variables[var]->value() = j;
				gm.variables[var]->addr_value() = j;
				break;
			}
		}
		assert(val_set);
	}
	assert((int)gm.csp_to_sat_variables[csp_variable].size()==gm.variables[csp_variable]->domain_size());
	int default_value = INVALID_VALUE;
	for (int i = 0; i < gm.variables[csp_variable]->domain_size(); i++) {
		if (toLbool(assignment[gm.csp_to_sat_variables[csp_variable][i]]) == l_False) {
			continue;
		}
		default_value = i;
		gm.variables[csp_variable]->addr_value() = i;
		int address = Variable::getAddress(function->variables());
		func_table[i] = function->table()[address];
		norm_const += func_table[i];
	}
	assert(default_value!=INVALID_VALUE);
	for (int i = 0; i < gm.variables[csp_variable]->domain_size(); i++) {
		func_table[i] /= norm_const;
	}
	double rand_val = random.getDouble();
	int sampled_value = INVALID_VALUE;
	for (int i = 0; i < gm.variables[csp_variable]->domain_size(); i++) {
		cdf += func_table[i];
		if (cdf.value() >= rand_val) {
			sampled_value = i;
			//cerr<<func_table[i]<<" ";
			break;
		}
	}
	if (sampled_value == INVALID_VALUE) {
		sampled_value = default_value;
	}

	assert(sampled_value!=INVALID_VALUE);
	variable = gm.csp_to_sat_variables[csp_variable][sampled_value];
	value = false;

}

void SampleSearch::getWeight(vec<lbool>& assignment, vector<Double>& bf)
{
	//Double sample_weight(1.0),actual_weight(1.0);
	Solver solver;
	gzFile in = gzopen(satfile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	vec<Lit> assumptions;
	bf = vector<Double> (gm.variables.size());
	//cout<<assignment.size()<<endl;
	// Set value
	for (int v = 0; v < gm.variables.size(); v++) {
		int var = order[v];
		//cout<<v<<" "<<flush;
		vector<Double> func_table(gm.variables[var]->domain_size());

		Function& function = helper.getFunction(var);
		//SF& function=
		for (int i = 0; i < gm.csp_to_sat_variables[var].size(); i++) {
			gm.variables[var]->addr_value() = i;
			int address = Variable::getAddress(function.variables());
			func_table[i] = function.table()[address];
		}
		bool val_set = false;

		for (int i = 0; i < gm.csp_to_sat_variables[var].size(); i++) {
			//cout<<gm.csp_to_sat_variables[var][i]<<" "<<flush;

			if (assignment[gm.csp_to_sat_variables[var][i]] == l_True) {
				val_set = true;
				gm.variables[var]->addr_value() = i;
				gm.variables[var]->value() = i;

				//continue;
			}
			else {
				assumptions.push((Lit(gm.csp_to_sat_variables[var][i])));
				//gzFile in = gzopen(satfile, "rb");
				//parse_DIMACS(in,solver);
				//cout<<"checking --- "<<flush;
				if (!solver.solve(assumptions)) {
					func_table[i] = Double();
				}
				//cout<<"Done\n";
				assumptions.pop();
			}
		}
		assert(val_set);
		Double norm_const;
		// Normalize the fun_table
		for (int i = 0; i < func_table.size(); i++) {
			norm_const += func_table[i];

		}
		bf[var] = (func_table[gm.variables[var]->addr_value()] / norm_const);
		//sample_weight*=(func_table[gm.variables[var]->addr_value()]/norm_const);

		//cout<<sample_weight<<" "<<func_table[gm.variables[var]->addr_value()]/norm_const<<endl;
		assumptions.push(Lit(gm.csp_to_sat_variables[var][gm.variables[var]->addr_value()]));
	}

	//for(int i=0;i<gm.functions.size();i++)
	//{
	//	actual_weight*=gm.functions[i]->getWeight();
	//}
	////cout<<"------"<<sample_weight<<" "<<actual_weight<<endl;
	//return actual_weight/sample_weight;
}

Double SampleSearch::getWeight(vec<lbool>& assignment)
{
	Double sample_weight(1.0), actual_weight(1.0);
	Solver solver;
	gzFile in = gzopen(satfile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	vec<Lit> assumptions;
	//cout<<assignment.size()<<endl;
	// Set value
	for (int v = 0; v < gm.variables.size(); v++) {
		int var = order[v];
		//cout<<v<<" "<<flush;
		vector<Double> func_table(gm.variables[var]->domain_size());

		Function& function = helper.getFunction(var);
		//SF& function=
		for (int i = 0; i < gm.csp_to_sat_variables[var].size(); i++) {
			gm.variables[var]->addr_value() = i;
			int address = Variable::getAddress(function.variables());
			func_table[i] = function.table()[address];
		}
		bool val_set = false;

		for (int i = 0; i < gm.csp_to_sat_variables[var].size(); i++) {
			//cout<<gm.csp_to_sat_variables[var][i]<<" "<<flush;

			if (assignment[gm.csp_to_sat_variables[var][i]] == l_True) {
				val_set = true;
				gm.variables[var]->addr_value() = i;
				gm.variables[var]->value() = i;

				//continue;
			}
			else {
				assumptions.push((Lit(gm.csp_to_sat_variables[var][i])));
				//gzFile in = gzopen(satfile, "rb");
				//parse_DIMACS(in,solver);
				//cout<<"checking --- "<<flush;
				if (!solver.solve(assumptions)) {
					func_table[i] = Double();
				}
				//cout<<"Done\n";
				assumptions.pop();
			}
		}
		assert(val_set);
		Double norm_const;
		// Normalize the fun_table
		for (int i = 0; i < func_table.size(); i++) {
			norm_const += func_table[i];

		}

		sample_weight *= (func_table[gm.variables[var]->addr_value()] / norm_const);

		//cout<<sample_weight<<" "<<func_table[gm.variables[var]->addr_value()]/norm_const<<endl;
		assumptions.push(Lit(gm.csp_to_sat_variables[var][gm.variables[var]->addr_value()]));
	}

	for (int i = 0; i < gm.functions.size(); i++) {
		actual_weight *= gm.functions[i]->getWeight();
	}
	//cout<<"------"<<sample_weight<<" "<<actual_weight<<endl;
	return actual_weight / sample_weight;
}

RBSampleSearch::RBSampleSearch(ostream& out_) :
	out(out_), interval(10)
{
	timer.start();
}

void RBSampleSearch::reduce(GM &gm, std::vector<vector<bool> >& new_domains)
{
	// The following code can be made much much efficient (Later :)- )
	new_domains = vector<vector<bool> > (gm.variables.size());
	for (int i = 0; i < new_domains.size(); i++)
		new_domains[i] = vector<bool> (gm.variables[i]->domain_size());
	Solver solver;
	setupSolver(solver, gm);

	cerr << "Solver inited\n";
	for (int i = 0; i < new_domains.size(); i++) {
		for (int j = 0; j < new_domains[i].size(); j++) {
			vec<Lit> assumptions;
			assumptions.push((Lit(gm.csp_to_sat_variables[i][j])));
			if (solver.solve(assumptions)) {
				new_domains[i][j] = true;
			}
		}
	}
	cerr << "Domains reduced\n";
}

pair<long double, long double> purgeORStore(GM& gm, IJGPSamplingHelper& helper, ORStore& store, vector<int>& sampling_order, vector<vector<int> >& all_samples, vector<
		long double>& all_weights, ostream& out)
{

	assert(all_samples.size()==all_weights.size());
	long double overall_weight_ub = 0.0;
	long double overall_weight_lb = 0.0;
	for (int i = 0; i < all_samples.size(); i++) {
		long double sample_weight_lb = 0.0, sample_weight_ub = 0.0;
		for (int j = 0; j < sampling_order.size(); j++) {
			gm.variables[sampling_order[j]]->value() = INVALID_VALUE;
		}
		assert(sampling_order.size() == all_samples[i].size());
		assert(store.next_pointer==0);
		for (int j = 0; j < all_samples[i].size(); j++) {
			int var = sampling_order[j];
			int val = all_samples[i][j];
			vector<Double> func_table_lb(gm.variables[var]->domain_size());
			vector<Double> func_table_ub(gm.variables[var]->domain_size());
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				func_table_lb[k] = Double();
				func_table_ub[k] = Double();
				if (store.check(var, k, false) != INCONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_ub[k] = function.table()[address];
				}
				if (store.check(var, k, false) == CONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_lb[k] = function.table()[address];
				}
			}
			//Advance next_pointer
			store.check(var, val, true);
			gm.variables[var]->value() = val;
			//Normalize function table
			normalize_table(func_table_lb);
			normalize_table(func_table_ub);
			assert(!func_table_lb[val].isZero());
			assert(!func_table_ub[val].isZero());
			sample_weight_lb += log(func_table_lb[val].value());
			sample_weight_ub += log(func_table_ub[val].value());

		}
		overall_weight_lb += exp(all_weights[i] - sample_weight_lb);
		overall_weight_ub += exp(all_weights[i] - sample_weight_ub);
	}
	for (int j = 0; j < sampling_order.size(); j++) {
		gm.variables[sampling_order[j]]->value() = INVALID_VALUE;
	}

	return pair<long double, long double> (overall_weight_lb, overall_weight_ub);
}
void purgeORStoreMarginals(GM& gm, IJGPSamplingHelper& helper, ORStore& store, vector<int>& sampling_order, vector<int>& rem_order, vector<vector<int> >& all_samples,
		vector<vector<int> >& rem_samples, vector<long double>& all_weights, vector<vector<Double> >& marginals_lb, vector<vector<Double> >& marginals_ub, ostream& out,
		bool to_write)
{

	assert(all_samples.size()==all_weights.size());
	long double overall_weight_ub = 0.0;
	long double overall_weight_lb = 0.0;
	for (int i = 0; i < all_samples.size(); i++) {
		long double sample_weight_lb = 0.0, sample_weight_ub = 0.0;
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}
		assert(sampling_order.size() == all_samples[i].size());
		assert(store.next_pointer==0);
		for (int j = 0; j < all_samples[i].size(); j++) {
			int var = sampling_order[j];
			int val = all_samples[i][j];
			vector<Double> func_table_lb(gm.variables[var]->domain_size());
			vector<Double> func_table_ub(gm.variables[var]->domain_size());
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				func_table_lb[k] = Double();
				func_table_ub[k] = Double();
				if (store.check(var, k, false) != INCONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_ub[k] = function.table()[address];
				}
				if (store.check(var, k, false) == CONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_lb[k] = function.table()[address];
				}
			}
			//Advance next_pointer
			store.check(var, val, true);
			gm.variables[var]->value() = val;
			//Normalize function table
			normalize_table(func_table_lb);
			normalize_table(func_table_ub);
			assert(!func_table_lb[val].isZero());
			assert(!func_table_ub[val].isZero());
			sample_weight_lb += log(func_table_lb[val].value());
			sample_weight_ub += log(func_table_ub[val].value());

		}
		// Set the remaining variables
		for (int j = 0; j < rem_order.size(); j++) {
			gm.variables[rem_order[j]]->value() = rem_samples[i][j];
		}

		// Now that all variables are set update the marginals
		for (int j = 0; j < gm.variables.size(); j++) {
			assert(gm.variables[j]->value()!=INVALID_VALUE);
			marginals_lb[j][gm.variables[j]->value()] += Double(exp(all_weights[i] - sample_weight_lb));
			marginals_ub[j][gm.variables[j]->value()] += Double(exp(all_weights[i] - sample_weight_ub));
		}
	}

	for (int j = 0; j < gm.variables.size(); j++) {
		gm.variables[j]->value() = INVALID_VALUE;
	}
	if (to_write) {
		gm.printMarginalsUAI10(marginals_ub, out);
	}
	//gm.printMarginalsUAI08(marginals_lb);
	//gm.printMarginalsUAI08(marginals_ub);
}

vector<int> topological_order;
vector<int> projected_topological_order;
void updateMarginals(GM& gm, GM& other_gm, vector<vector<Double> >& marginals, myRandom& random)
{
	if (topological_order.empty()) {
		Graph graph;
		graph.makeDirectedGraph(&other_gm);
		graph.getTopologicalOrdering(topological_order);
		vector<bool> to_keep(topological_order.size());
		for (int i = 0; i < topological_order.size(); i++) {
			int var = topological_order[i];
			if (other_gm.variables[var]->value() == INVALID_VALUE) {
				to_keep[var] = true;
			}
			else {
				to_keep[var] = false;
			}
		}
		for (int i = 0; i < gm.variables.size(); i++) {
			assert(gm.variables[i]->value()!=INVALID_VALUE);
			int value = gm.variables[i]->mapping[gm.variables[i]->value()];
			to_keep[gm.variables[i]->orig_id] = false;
		}
		for (int i = 0; i < topological_order.size(); i++) {
			if (to_keep[topological_order[i]]) {
				projected_topological_order.push_back(topological_order[i]);
			}
		}

	}
	vector<int> assigned_vars;
	for (int i = 0; i < gm.functions.size(); i++) {
		for (int j = 0; j < gm.functions[i]->variables().size(); j++) {
			assert(gm.functions[i]->variables()[j]->value()!=INVALID_VALUE);
		}
		assert(!gm.functions[i]->table()[Variable::getAddress(gm.functions[i]->variables())].isZero());
	}
	//Update the values of variables 
	for (int i = 0; i < gm.variables.size(); i++) {
		assert(gm.variables[i]->value()!=INVALID_VALUE);
		int value = gm.variables[i]->mapping[gm.variables[i]->value()];
		other_gm.variables[gm.variables[i]->orig_id]->value() = value;
		assigned_vars.push_back(gm.variables[i]->orig_id);
	}
	// Now sample in topological order
	//for(int i=0;i<other_gm.variables.size();i++){
	for (int i = 0; i < projected_topological_order.size(); i++) {
		int var = projected_topological_order[i];
		//if (other_gm.variables[var]->value()!=INVALID_VALUE){
		//	assert(!other_gm.functions[var]->table()[Variable::getAddress(other_gm.functions[var]->variables())].isZero());
		//	continue;
		//}
		assert(other_gm.variables[var]->value()==INVALID_VALUE);
		int value = INVALID_VALUE;
		vector<Double> marg(other_gm.variables[var]->domain_size());
		bool found = false;
		for (int j = 0; j < other_gm.functions[var]->variables().size(); j++) {
			if (other_gm.functions[var]->variables()[j]->id() == var) {
				found = true;
				continue;
			}
			assert(other_gm.functions[var]->variables()[j]->value()!=INVALID_VALUE);
		}
		assert(found);
		for (int j = 0; j < marg.size(); j++) {
			other_gm.variables[var]->addr_value() = j;
			int entry = Variable::getAddress(other_gm.functions[var]->variables());
			marg[j] = other_gm.functions[var]->table()[entry];
		}
		//generate sample
		other_gm.variables[var]->value() = generateSample(marg, random);
		assert(other_gm.variables[var]->value()!=INVALID_VALUE);
		assigned_vars.push_back(var);
		marginals[var][other_gm.variables[var]->value()] += Double(1.0);
	}
	Double mult_value(1.0);
	//Make sure that the sample has weight > 0
	for (int i = 0; i < other_gm.functions.size(); i++) {
		int entry = Variable::getAddress(other_gm.functions[i]->variables());
		mult_value *= other_gm.functions[i]->table()[entry];
	}
	assert(!mult_value.isZero());
	// Make sure that all variables are sampled
	for (int i = 0; i < other_gm.variables.size(); i++)
		assert(other_gm.variables[i]->value()!=INVALID_VALUE);
	// Unassign all variables that were assigned here
	for (int i = 0; i < assigned_vars.size(); i++) {
		other_gm.variables[assigned_vars[i]]->value() = INVALID_VALUE;
	}

}
void printMarginals(GM& gm, GM& other_gm, vector<vector<Double> >& marginals_lb, vector<vector<Double> >& marginals_ub, vector<vector<Double> >& other_marginals,
		ostream& out)
{
	cout << "Printing marginals\n";
	for (int i = 0; i < gm.variables.size(); i++) {
		int other_var = gm.variables[i]->orig_id;
		for (int j = 0; j < gm.variables[i]->domain_size(); j++) {
			int other_val = gm.variables[i]->mapping[j];
			other_marginals[other_var][other_val] = marginals_lb[i][j];
		}
	}
	vector<vector<Double> > marginals = other_marginals;
	out << marginals.size() << " ";
	for (int i = 0; i < marginals.size(); i++) {
		out << marginals[i].size() << " ";
		normalize_table(marginals[i]);
		for (int j = 0; j < marginals[i].size(); j++) {
			out << marginals[i][j] << " ";
		}
	}
	out << endl;
	/*
	 out<<"m "<<marginals.size()<<" ";
	 for(int i=0;i<marginals.size();i++){
	 out<<marginals[i].size()<<" ";
	 normalize_table(marginals[i]);
	 for(int j=0;j<marginals[i].size();j++){
	 out<<marginals[i][j]<<" ";
	 }
	 }
	 out<<endl;

	 for(int i=0;i<gm.variables.size();i++){
	 int other_var=gm.variables[i]->orig_id;
	 for(int j=0;j<gm.variables[i]->domain_size();j++)
	 {
	 int other_val=gm.variables[i]->mapping[j];
	 other_marginals[other_var][other_val]=marginals_ub[i][j];
	 }
	 }
	 marginals=other_marginals;
	 out<<"m "<<marginals.size()<<" ";
	 for(int i=0;i<marginals.size();i++){
	 out<<marginals[i].size()<<" ";
	 normalize_table(marginals[i]);
	 for(int j=0;j<marginals[i].size();j++){
	 out<<marginals[i][j]<<" ";
	 }
	 }
	 out<<endl;
	 */
}

void purgeORStoreBayesMarginals(GM& gm, GM& other_gm, IJGPSamplingHelper& helper, ORStore& store, vector<int>& sampling_order, vector<int>& rem_order,
		vector<vector<int> >& all_samples, vector<vector<int> >& rem_samples, vector<long double>& all_weights, vector<vector<Double> >& marginals_lb, vector<vector<
				Double> >& marginals_ub, vector<vector<Double> >& other_marginals, myRandom& random, ostream& out, bool to_write)
{

	assert(all_samples.size()==all_weights.size());
	long double overall_weight_ub = 0.0;
	long double overall_weight_lb = 0.0;
	for (int i = 0; i < all_samples.size(); i++) {
		long double sample_weight_lb = 0.0, sample_weight_ub = 0.0;
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}
		assert(sampling_order.size() == all_samples[i].size());
		assert(store.next_pointer==0);
		for (int j = 0; j < all_samples[i].size(); j++) {
			int var = sampling_order[j];
			int val = all_samples[i][j];
			vector<Double> func_table_lb(gm.variables[var]->domain_size());
			vector<Double> func_table_ub(gm.variables[var]->domain_size());
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				func_table_lb[k] = Double();
				func_table_ub[k] = Double();
				if (store.check(var, k, false) != INCONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_ub[k] = function.table()[address];
				}
				if (store.check(var, k, false) == CONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_lb[k] = function.table()[address];
				}
			}
			//Advance next_pointer
			store.check(var, val, true);
			gm.variables[var]->value() = val;
			//Normalize function table
			normalize_table(func_table_lb);
			normalize_table(func_table_ub);
			assert(!func_table_lb[val].isZero());
			assert(!func_table_ub[val].isZero());
			sample_weight_lb += log(func_table_lb[val].value());
			sample_weight_ub += log(func_table_ub[val].value());

		}
		// Set the remaining variables
		for (int j = 0; j < rem_order.size(); j++) {
			gm.variables[rem_order[j]]->value() = rem_samples[i][j];
		}
		for (int j = 0; j < gm.variables.size(); j++) {
			assert(gm.variables[j]->value()!=INVALID_VALUE);
			marginals_lb[j][gm.variables[j]->value()] += Double(exp(all_weights[i] - sample_weight_lb));
			marginals_ub[j][gm.variables[j]->value()] += Double(exp(all_weights[i] - sample_weight_ub));
		}
		updateMarginals(gm, other_gm, other_marginals, random);
	}
	for (int j = 0; j < gm.variables.size(); j++) {
		gm.variables[j]->value() = INVALID_VALUE;
	}
	if (to_write) {
		printMarginals(gm, other_gm, marginals_lb, marginals_ub, other_marginals, out);
	}
}

void RBSampleSearch::computePEApp(GM& gm, JG& jg, int p_bound, vector<int>& order, clock_t& time_bound, vector<int>& sampling_order, int max_restarts)
{

	Solver solver;
	// Initialize the SAT solver
	setupSolver(solver, gm, true);
	vector<bool> is_sampled(order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		is_sampled[sampling_order[i]] = true;
	vector<int> helper_order;
	vector<int> helper_sampling_order;

	for (int i = 0; i < order.size(); i++)
		if (!is_sampled[order[i]])
			helper_order.push_back(order[i]);
		else
			helper_sampling_order.push_back(order[i]);
	for (int i = 0; i < helper_sampling_order.size(); i++)
		helper_order.push_back(helper_sampling_order[i]);
	sampling_order = vector<int> ();
	for (int i = helper_sampling_order.size() - 1; i > -1; i--)
		sampling_order.push_back(helper_sampling_order[i]);
	assert(helper_order.size()==order.size());
	IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order, POSITIVE_SAMPLER);
	myRandom random;
	long double weight_lb = 0.0, weight_ub = 0.0;
	time_t curr_time;

	// Initialize the sampling order
	ORStore store(gm, sampling_order);
	vector<vector<int> > all_samples;
	vector<long double> all_weights;
	bool timeout = false;
	//for(int i=0;i<num_samples;i++)
	int curr_num_samples = 0;
	long double num_samples = 0;
	while (1) {

		// Set all variables to invalid value
		for (int j = 0; j < gm.variables.size(); j++)
			gm.variables[j]->value() = INVALID_VALUE;
		vec<Lit> assumptions;
		all_samples.push_back(vector<int> ());
		int cs = all_samples.size() - 1;
		long double sample_weight = 0.0;
		assert(store.next_pointer == 0);
		// Sample all variables according to the sampling order
		for (int j = 0; j < sampling_order.size(); j++) {

			int var = sampling_order[j];
			vector<Double> func_table(gm.variables[var]->domain_size());
			assert(gm.variables[var]->value()==INVALID_VALUE);
			// Initialize the proposal distribution for sampling
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				gm.variables[var]->addr_value() = k;
				int address = Variable::getAddress(function.variables());
				func_table[k] = function.table()[address];
			}

			// Update the OR store
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				if (!func_table[k].isZero()) {
					if (store.check(var, k, false) == DONTKNOW) {
						assumptions.push(Lit(gm.csp_to_sat_variables[var][k]));
						lbool solver_status = solver.solve(max_restarts, assumptions);
						if (solver_status == l_True)
							store.AddGood(var, k);
						else if (solver_status == l_False) {
							store.AddNoGood(var, k);
						}
						assumptions.pop();
					}
				}
				else {
					store.AddNoGood(var, k);
				}
			}

			while (1) {
				int value = generateSample(func_table, random, true);
				assert(value!=INVALID_VALUE);
				assumptions.push(Lit(gm.csp_to_sat_variables[var][value]));
				if (isConsistent(var, value, assumptions, store, solver)) {
					all_samples[cs].push_back(value);
					gm.variables[var]->value() = value;
					sample_weight += log(func_table[value].value());
					break;
				}
				else {
					func_table[value] = Double();
					assumptions.pop();
				}
			}
		}
		BE be(gm.variables, gm.functions, order);
		//long double actual_weight=log(be.log_pe.toDouble().value());
		long double actual_weight = be.log_pe.toLongdouble();
		//cout<<exp(actual_weight-sample_weight)<<endl;
		all_weights.push_back(actual_weight);
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}

		//Output z value after 10000 samples are generated
		curr_num_samples++;
		num_samples += 1;
		bool time_out = timer.timeout(time_bound);
		if (time_out || curr_num_samples == interval) {
			curr_num_samples = 0;
			pair<long double, long double> tmp;
			tmp = purgeORStore(gm, helper, store, sampling_order, all_samples, all_weights, out);

			long double pe_lb = (weight_lb + tmp.first) / (long double) num_samples;

			pe_lb *= gm.mult_factor.value();

			long double pe_ub = (weight_ub + tmp.second) / (long double) num_samples;
			pe_ub *= gm.mult_factor.value();
			//out<<log10(pe_ub)<<endl;
			//cout << log10(pe_ub) << endl;
			if (tmp.first == tmp.second) {
				all_samples = vector<vector<int> > ();
				all_weights = vector<long double> ();
				weight_lb += tmp.first;
				weight_ub += tmp.second;
				tmp.first = 0.0;
				tmp.second = 0.0;
			}
			if ((int) store.storage.size() > 234217728 || time_out) {
				all_samples = vector<vector<int> > ();
				all_weights = vector<long double> ();
				weight_lb += tmp.first;
				weight_ub += tmp.second;
				store.reset();
			}
			if (time_out) {

				for (int j = 0; j < gm.variables.size(); j++) {
					gm.variables[j]->value() = INVALID_VALUE;
				}
				out << log10((pe_ub + pe_lb) / (long double) 2) << endl;
				cout << num_samples << endl;
				return;
				//return log10((pe_ub+pe_lb)/(long double)2);
			}
		}
	}
}
void RBSampleSearch::computeBeliefsApp(GM& gm, JG& jg, int p_bound, vector<int>& order, clock_t& time_bound, vector<int>& sampling_order, int max_restarts)
{
	// Initialize the SAT solver

	Solver solver;
	setupSolver(solver, gm);
	vector<bool> is_sampled(order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		is_sampled[sampling_order[i]] = true;
	vector<int> rem_order;
	for (int i = 0; i < gm.variables.size(); i++) {
		if (!is_sampled[i]) {
			rem_order.push_back(i);
		}
	}
	vector<int> helper_order;
	vector<int> helper_sampling_order;

	for (int i = 0; i < order.size(); i++)
		if (!is_sampled[order[i]])
			helper_order.push_back(order[i]);
		else
			helper_sampling_order.push_back(order[i]);
	for (int i = 0; i < helper_sampling_order.size(); i++)
		helper_order.push_back(helper_sampling_order[i]);
	sampling_order = vector<int> ();
	for (int i = helper_sampling_order.size() - 1; i > -1; i--)
		sampling_order.push_back(helper_sampling_order[i]);
	assert((sampling_order.size()+rem_order.size())==gm.variables.size());
	assert(helper_order.size()==order.size());
	IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order, POSITIVE_SAMPLER);
	myRandom random;
	long double weight_lb = 0.0, weight_ub = 0.0;
	time_t curr_time;

	// Initialize the sampling order
	ORStore store(gm, sampling_order);
	vector<vector<int> > all_samples;
	vector<vector<int> > rem_samples;
	vector<long double> all_weights;
	vector<vector<Double> > marginals_lb(gm.variables.size());
	vector<vector<Double> > marginals_ub(gm.variables.size());

	for (int i = 0; i < gm.variables.size(); i++) {
		marginals_lb[i] = vector<Double> (gm.variables[i]->domain_size());
		marginals_ub[i] = vector<Double> (gm.variables[i]->domain_size());
	}
	int curr_num_samples = 0;
	while (1) {
		// Set all variables to invalid value
		for (int j = 0; j < gm.variables.size(); j++)
			gm.variables[j]->value() = INVALID_VALUE;

		vec<Lit> assumptions;
		all_samples.push_back(vector<int> ());
		int cs = all_samples.size() - 1;
		long double sample_weight = 0.0;
		assert(store.next_pointer == 0);
		// Sample all variables according to the sampling order
		for (int j = 0; j < sampling_order.size(); j++) {
			int var = sampling_order[j];
			vector<Double> func_table(gm.variables[var]->domain_size());
			assert(gm.variables[var]->value()==INVALID_VALUE);
			// Initialize the proposal distribution for sampling
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				gm.variables[var]->addr_value() = k;
				int address = Variable::getAddress(function.variables());
				func_table[k] = function.table()[address];
			}

			// Update the OR store
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				if (!func_table[k].isZero()) {
					if (store.check(var, k, false) == DONTKNOW) {
						assumptions.push(Lit(gm.csp_to_sat_variables[var][k]));
						lbool solver_status = solver.solve(max_restarts, assumptions);
						if (solver_status == l_True)
							store.AddGood(var, k);
						else if (solver_status == l_False) {
							store.AddNoGood(var, k);
						}
						assumptions.pop();
					}
				}
				else {
					store.AddNoGood(var, k);
				}
			}

			while (1) {
				int value = generateSample(func_table, random, true);
				assert(value!=INVALID_VALUE);
				assumptions.push(Lit(gm.csp_to_sat_variables[var][value]));
				if (isConsistent(var, value, assumptions, store, solver)) {
					all_samples[cs].push_back(value);
					gm.variables[var]->value() = value;
					sample_weight += log(func_table[value].value());
					break;
				}
				else {
					func_table[value] = Double();
					assumptions.pop();
				}
			}
		}
		BESample be_sample(gm.variables, gm.functions, order, random);
		//long double actual_weight=log(be.log_pe.toDouble().value());
		long double actual_weight = be_sample.log_pe.toLongdouble();
		rem_samples.push_back(vector<int> ());
		int rcs = rem_samples.size() - 1;
		for (int j = 0; j < rem_order.size(); j++)
			rem_samples[rcs].push_back(gm.variables[rem_order[j]]->value());

		all_weights.push_back(actual_weight);
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}

		curr_num_samples++;
		bool time_out = timer.timeout(time_bound);
		if (time_out || curr_num_samples == 0) {
			curr_num_samples = 0;
			purgeORStoreMarginals(gm, helper, store, sampling_order, rem_order, all_samples, rem_samples, all_weights, marginals_lb, marginals_ub, out, time_out);
			if ((int) store.storage.size() > 104217728 || time_out) {
				all_samples = vector<vector<int> > ();
				rem_samples = vector<vector<int> > ();
				all_weights = vector<long double> ();
				store.reset();
			}
			if (time_out) {
				return;
			}
		}
	}
}
void RBSampleSearch::computeBeliefsApp_Improved(GM& gm, JG& jg, int p_bound, vector<int>& order, clock_t& time_bound, vector<int>& sampling_order, int max_restarts,
		vector<set<int> >& clusters)
{
	// Initialize the SAT solver

	Solver solver;
	setupSolver(solver, gm);
	vector<bool> is_sampled(order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		is_sampled[sampling_order[i]] = true;
	vector<int> rem_order;
	for (int i = 0; i < gm.variables.size(); i++) {
		if (!is_sampled[i]) {
			rem_order.push_back(i);
		}
	}
	vector<int> helper_order;
	vector<int> helper_sampling_order;

	for (int i = 0; i < order.size(); i++)
		if (!is_sampled[order[i]])
			helper_order.push_back(order[i]);
		else
			helper_sampling_order.push_back(order[i]);
	for (int i = 0; i < helper_sampling_order.size(); i++)
		helper_order.push_back(helper_sampling_order[i]);
	sampling_order = vector<int> ();
	for (int i = helper_sampling_order.size() - 1; i > -1; i--)
		sampling_order.push_back(helper_sampling_order[i]);
	assert((sampling_order.size()+rem_order.size())==gm.variables.size());
	assert(helper_order.size()==order.size());
	IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order, POSITIVE_SAMPLER);
	myRandom random;
	long double weight_lb = 0.0, weight_ub = 0.0;
	time_t curr_time;

	// Initialize the sampling order
	ORStore store(gm, sampling_order);

	vector<vector<Double> > marginals(gm.variables.size());

	for (int i = 0; i < gm.variables.size(); i++) {
		marginals[i] = vector<Double> (gm.variables[i]->domain_size());
	}
	int curr_num_samples = 0;
	JT jt(gm.variables, gm.functions, clusters, order);
	for (int j = 0; j < gm.variables.size(); j++)
		gm.variables[j]->value() = INVALID_VALUE;
	while (1) {
		vec<Lit> assumptions;
		long double sample_weight = 0.0;
		assert(store.next_pointer == 0);

		// Sample all variables according to the sampling order
		for (int j = 0; j < sampling_order.size(); j++) {
			int var = sampling_order[j];
			vector<Double> func_table(gm.variables[var]->domain_size());
			assert(gm.variables[var]->value()==INVALID_VALUE);
			// Initialize the proposal distribution for sampling
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				gm.variables[var]->addr_value() = k;
				int address = Variable::getAddress(function.variables());
				func_table[k] = function.table()[address];
			}

			// Update the OR store
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				if (!func_table[k].isZero()) {
					if (store.check(var, k, false) == DONTKNOW) {
						assumptions.push(Lit(gm.csp_to_sat_variables[var][k]));
						lbool solver_status = solver.solve(max_restarts, assumptions);
						if (solver_status == l_True)
							store.AddGood(var, k);
						else if (solver_status == l_False) {
							store.AddNoGood(var, k);
						}
						assumptions.pop();
					}
				}
				else {
					store.AddNoGood(var, k);
				}
			}

			while (1) {
				int value = generateSample(func_table, random, true);
				assert(value!=INVALID_VALUE);
				assumptions.push(Lit(gm.csp_to_sat_variables[var][value]));
				if (isConsistent(var, value, assumptions, store, solver)) {
					gm.variables[var]->value() = value;
					sample_weight += log(func_table[value].value());
					break;
				}
				else {
					func_table[value] = Double();
					assumptions.pop();
				}
			}
		}
		BE be(gm.variables, gm.functions, order);
		long double actual_weight = be.log_pe.toLongdouble();
		jt.propagate();
		Double weight(exp(actual_weight - sample_weight));
		assert(marginals.size()==jt.marginals.size());
		for (int i = 0; i < marginals.size(); i++) {
			if (is_sampled[i]) {
				marginals[i][gm.variables[i]->value()] += weight;
			}
			else {
				for (int j = 0; j < gm.variables[i]->domain_size(); j++) {
					marginals[i][j] += jt.marginals[i].table()[j] * weight;
				}
			}
		}
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}
		curr_num_samples++;
		if ((int) store.storage.size() > 104217728) {
			store.reset();
		}
		bool time_out = timer.timeout(time_bound);
		if (time_out) {
			cout << "Time taken = " << timer.elapsed_seconds() << endl;
			cout << "Number of samples = " << curr_num_samples << endl;
			store.reset();
			gm.printMarginalsUAI10(marginals, out);
			return;
		}
	}
}

void RBSampleSearch::computeBayesBeliefsApp(GM& gm, GM& other_gm, JG& jg, int p_bound, vector<int>& order, clock_t& time_bound, vector<int>& sampling_order,
		int max_restarts)
{

	timer.start();
	int curr_num_samples = 0;
	Solver solver;
	setupSolver(solver, gm, true);
	vector<bool> is_sampled(order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		is_sampled[sampling_order[i]] = true;
	vector<int> rem_order;
	for (int i = 0; i < gm.variables.size(); i++) {
		if (!is_sampled[i]) {
			rem_order.push_back(i);
		}
	}
	vector<int> helper_order;
	vector<int> helper_sampling_order;

	for (int i = 0; i < order.size(); i++)
		if (!is_sampled[order[i]])
			helper_order.push_back(order[i]);
		else
			helper_sampling_order.push_back(order[i]);
	//for(int i=(int)sampling_order.size()-1;i>-1;i--)
	//	helper_order.push_back(sampling_order[i]);
	for (int i = 0; i < helper_sampling_order.size(); i++)
		helper_order.push_back(helper_sampling_order[i]);
	sampling_order = vector<int> ();
	for (int i = helper_sampling_order.size() - 1; i > -1; i--)
		sampling_order.push_back(helper_sampling_order[i]);
	assert((sampling_order.size()+rem_order.size())==gm.variables.size());
	assert(helper_order.size()==order.size());
	IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order);
	myRandom random;
	long double weight_lb = 0.0, weight_ub = 0.0;
	time_t curr_time;

	// Initialize the sampling order
	ORStore store(gm, sampling_order);
	vector<vector<int> > all_samples;
	vector<vector<int> > rem_samples;
	vector<long double> all_weights;
	vector<vector<Double> > marginals_lb(gm.variables.size());
	vector<vector<Double> > marginals_ub(gm.variables.size());

	vector<vector<Double> > other_marginals(other_gm.variables.size());
	for (int i = 0; i < other_gm.variables.size(); i++) {
		other_marginals[i] = vector<Double> (other_gm.variables[i]->domain_size());
		//update marginals of evidence variables
		if (other_gm.variables[i]->value() != INVALID_VALUE) {
			other_marginals[i][other_gm.variables[i]->value()] = Double(1.0);
		}
	}
	for (int i = 0; i < gm.variables.size(); i++) {
		marginals_lb[i] = vector<Double> (gm.variables[i]->domain_size());
		marginals_ub[i] = vector<Double> (gm.variables[i]->domain_size());
	}

	while (1) {
		// Set all variables to invalid value
		for (int j = 0; j < gm.variables.size(); j++)
			gm.variables[j]->value() = INVALID_VALUE;

		vec<Lit> assumptions;
		all_samples.push_back(vector<int> ());
		int cs = all_samples.size() - 1;
		long double sample_weight = 0.0;
		assert(store.next_pointer == 0);
		// Sample all variables according to the sampling order
		for (int j = 0; j < sampling_order.size(); j++) {
			int var = sampling_order[j];
			vector<Double> func_table(gm.variables[var]->domain_size());
			assert(gm.variables[var]->value()==INVALID_VALUE);
			// Initialize the proposal distribution for sampling
			Function& function = helper.getFunction(var);
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				gm.variables[var]->addr_value() = k;
				int address = Variable::getAddress(function.variables());
				func_table[k] = function.table()[address];
			}

			// Update the OR store
			for (int k = 0; k < gm.csp_to_sat_variables[var].size(); k++) {
				if (!func_table[k].isZero()) {
					if (store.check(var, k, false) == DONTKNOW) {
						assumptions.push(Lit(gm.csp_to_sat_variables[var][k]));
						lbool solver_status = solver.solve(max_restarts, assumptions);
						if (solver_status == l_True)
							store.AddGood(var, k);
						else if (solver_status == l_False) {
							store.AddNoGood(var, k);
						}
						assumptions.pop();
					}
				}
				else {
					store.AddNoGood(var, k);
				}
			}

			while (1) {
				int value = generateSample(func_table, random, true);
				assert(value!=INVALID_VALUE);
				assumptions.push(Lit(gm.csp_to_sat_variables[var][value]));
				if (isConsistent(var, value, assumptions, store, solver)) {
					all_samples[cs].push_back(value);
					gm.variables[var]->value() = value;
					sample_weight += log(func_table[value].value());
					break;
				}
				else {
					func_table[value] = Double();
					assumptions.pop();
				}
			}
		}
		BESample be_sample(gm.variables, gm.functions, order, random);
		long double actual_weight = be_sample.log_pe.toLongdouble();
		rem_samples.push_back(vector<int> ());
		int rcs = rem_samples.size() - 1;
		for (int j = 0; j < rem_order.size(); j++)
			rem_samples[rcs].push_back(gm.variables[rem_order[j]]->value());

		all_weights.push_back(actual_weight);
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}
		curr_num_samples++;
		bool time_out = timer.timeout(time_bound);
		//Output z value after 1000 samples are generated
		if (curr_num_samples == interval || time_out) {
			curr_num_samples = 0;
			purgeORStoreBayesMarginals(gm, other_gm, helper, store, sampling_order, rem_order, all_samples, rem_samples, all_weights, marginals_lb, marginals_ub,
					other_marginals, random, out, time_out);
			all_samples = vector<vector<int> > ();
			rem_samples = vector<vector<int> > ();
			all_weights = vector<long double> ();
			if ((int) store.storage.size() > 104217728 || time_out) {
				store.reset();
			}
			if (time_out) {
				for (int j = 0; j < gm.variables.size(); j++) {
					gm.variables[j]->value() = INVALID_VALUE;
				}
				return;
			}
		}
	}
}

bool SampleSearch::isconsistent(GM& gm)
{
	gm.convertToSAT();

	Solver solver;
	gzFile in = gzopen(satfile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	return solver.solve();
}

pair<double, double> purgeORStoreSAT(GM& gm, IJGPSamplingHelper& helper, ORStore& store, vector<int>& sampling_order, vector<vector<int> >& all_samples,
		vector<double>& all_weights, ofstream& out)
{

	assert(all_samples.size()==all_weights.size());
	out << "w " << " ";
	double overall_weight_ub = 0.0;
	double overall_weight_lb = 0.0;
	for (int i = 0; i < all_samples.size(); i++) {
		double sample_weight_lb = 0.0, sample_weight_ub = 0.0;
		for (int j = 0; j < sampling_order.size(); j++) {
			gm.variables[sampling_order[j]]->value() = INVALID_VALUE;
		}
		assert(sampling_order.size() == all_samples[i].size());
		for (int j = 0; j < all_samples[i].size(); j++) {
			int var = sampling_order[j];
			int val = all_samples[i][j];
			vector<Double> func_table_lb(gm.variables[var]->domain_size());
			vector<Double> func_table_ub(gm.variables[var]->domain_size());
			Function& function = helper.getFunction(var);
			for (int k = 0; k < 2; k++) {
				func_table_lb[k] = Double();
				func_table_ub[k] = Double();
				if (store.check(var, k, false) != INCONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_ub[k] = function.table()[address];
				}
				if (store.check(var, k, false) == CONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_lb[k] = function.table()[address];
				}
			}
			for (int k = 0; k < 2; k++) {
				if (func_table_lb[k].isZero())
					continue;
				if (func_table_ub[k].isZero())
					continue;
				if (func_table_lb[k] > Double(0.9)) {
					func_table_lb[k] = Double(0.9);
				}
				if (func_table_lb[k] < Double(0.1)) {
					func_table_lb[k] = Double(0.1);
				}
				if (func_table_lb[k] > Double(0.9)) {
					func_table_ub[k] = Double(0.9);
				}
				if (func_table_ub[k] < Double(0.1)) {
					func_table_ub[k] = Double(0.1);
				}
			}
			//Advance next_pointer
			store.check(var, val, true);
			gm.variables[var]->value() = val;
			//Normalize function table
			normalize_table(func_table_lb);
			normalize_table(func_table_ub);
			assert(!func_table_lb[val].isZero());
			assert(!func_table_ub[val].isZero());
			sample_weight_lb += log(func_table_lb[val].value());
			sample_weight_ub += log(func_table_ub[val].value());

		}
		out << exp(all_weights[i] - sample_weight_lb) * gm.mult_factor.value() << " ";
		out << exp(all_weights[i] - sample_weight_ub) * gm.mult_factor.value() << " ";

		//cout<<all_weights[i]<<" "<<sample_weight<<" "<<exp(all_weights[i]-sample_weight)<<endl;
		overall_weight_lb += exp(all_weights[i] - sample_weight_lb);
		overall_weight_ub += exp(all_weights[i] - sample_weight_ub);
	}
	out << endl;
	for (int j = 0; j < sampling_order.size(); j++) {
		gm.variables[sampling_order[j]]->value() = INVALID_VALUE;
	}
	//store.reset();
	return pair<double, double> (overall_weight_lb, overall_weight_ub);
}
RBSampleSearchSAT::RBSampleSearchSAT() :
	interval(10), outfilename("out")
{
}

Double RBSampleSearchSAT::computePEApp(GM&gm, JG& jg, const char* satfile, int p_bound, vector<int>& order, vector<int>& sampling_order, int num_samples,
		int max_restarts)
{
	// Initialize the SAT solver
	/*        SATInstance xInstance;
	 SATSolver xSATSolver(&xInstance);
	 char satfilename[1000];
	 strcpy(satfilename,satfile);
	 xInstance.bReadDimacs(satfilename);
	 xSATSolver.bPreprocess(3,10);
	 ofstream xNewInstance;
	 xNewInstance.open("temp.cnf", ios::out);
	 xInstance.vOutputDimacs(xNewInstance);
	 */
	Solver solver;
	gzFile in = gzopen("temp2.cnf", "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	ofstream out(outfilename.c_str());
	out << "t " << getDateAndTime() << endl;
	vector<bool> is_sampled(order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		is_sampled[sampling_order[i]] = true;
	vector<int> helper_order;
	vector<int> helper_sampling_order;

	for (int i = 0; i < order.size(); i++)
		if (!is_sampled[order[i]])
			helper_order.push_back(order[i]);
		else
			helper_sampling_order.push_back(order[i]);
	//for(int i=(int)sampling_order.size()-1;i>-1;i--)
	//	helper_order.push_back(sampling_order[i]);
	for (int i = 0; i < helper_sampling_order.size(); i++)
		helper_order.push_back(helper_sampling_order[i]);
	sampling_order = vector<int> ();
	for (int i = helper_sampling_order.size() - 1; i > -1; i--)
		sampling_order.push_back(helper_sampling_order[i]);
	assert(helper_order.size()==order.size());
	IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order);
	myRandom random;
	double weight_lb = 0.0, weight_ub = 0.0;
	time_t curr_time;

	// Initialize the sampling order
	ORStore store(gm, sampling_order);
	vector<vector<int> > all_samples;
	vector<double> all_weights;
	//double tmp_exact_weight=0.0;

	for (int i = 0; i < num_samples; i++) {
		// Set all variables to invalid value
		for (int j = 0; j < sampling_order.size(); j++)
			gm.variables[sampling_order[j]]->value() = INVALID_VALUE;

		vec<Lit> assumptions;
		all_samples.push_back(vector<int> ());
		int cs = all_samples.size() - 1;
		double sample_weight = 0.0;
		double tmp_sample_weight = 1.0;
		assert(store.next_pointer == 0);
		vec<Lit> tmp_assumptions;
		// Sample all variables according to the sampling order
		for (int j = 0; j < sampling_order.size(); j++) {
			//cout<<"Processing variable "<<j<<" out of "<<sampling_order.size()<<endl;

			int var = sampling_order[j];
			vector<Double> func_table(gm.variables[var]->domain_size());
			assert(gm.variables[var]->value()==INVALID_VALUE);
			// Initialize the proposal distribution for sampling
			Function& function = helper.getFunction(var);
			//vector<double> tmp_table(2);
			for (int k = 0; k < 2; k++) {
				gm.variables[var]->addr_value() = k;
				int address = Variable::getAddress(function.variables());
				func_table[k] = function.table()[address];
			}
			//for(int k=0;k<2;k++){
			//	tmp_table[k]=(func_table[k].value())/(func_table[0].value()+func_table[1].value());
			//}
			// Update the OR store
			if (store.check(var, 0, false) == DONTKNOW) {
				assumptions.push(~Lit(var));
				lbool solver_status = solver.solve(max_restarts, assumptions);
				if (solver_status == l_True)
					store.AddGood(var, 0);
				else if (solver_status == l_False) {
					store.AddNoGood(var, 0);
					store.AddGood(var, 1);
				}
				assumptions.pop();
			}
			if (store.check(var, 1, false) == DONTKNOW) {
				assumptions.push(Lit(var));
				lbool solver_status = solver.solve(max_restarts, assumptions);
				if (solver_status == l_True)
					store.AddGood(var, 1);
				else if (solver_status == l_False) {
					store.AddNoGood(var, 1);
					store.AddGood(var, 0);
				}
				assumptions.pop();
			}
			int value = -1;
			//Adjust func_table
			for (int k = 0; k < func_table.size(); k++) {
				if (func_table[k] > Double(0.9)) {
					func_table[k] = Double(0.9);
				}
				if (func_table[k] < Double(0.1)) {
					func_table[k] = Double(0.1);
				}
			}
			normalize_table(func_table);
			while (1) {
				value = generateSample(func_table, random, true);
				assert(value!=INVALID_VALUE);
				if (value == 0) {
					assumptions.push(~Lit(var));
				}
				else {
					assumptions.push(Lit(var));
				}
				if (isConsistentSAT(var, value, assumptions, store, solver)) {
					all_samples[cs].push_back(value);
					gm.variables[var]->value() = value;
					//cout<<"multiplier = "<<func_table[value].value()<<endl;
					break;
				}
				else {
					func_table[value] = Double();
					assumptions.pop();
				}
			}

			// Extra code for checking
			//			bool k0=true,k1=true;
			//			for(int k=0;k<2;k++){
			//				if (k==0){
			//					tmp_assumptions.push(~Lit(var));
			//					if (!solver.solve(tmp_assumptions)){
			//						k0=false;
			//					}
			//				}
			//				else{
			//					tmp_assumptions.push(Lit(var));
			//					if (!solver.solve(tmp_assumptions)){
			//						k1=false;
			//					}
			//				}
			//				tmp_assumptions.pop();
			//			}
			//			assert(k0 || k1);
			//			if (k0 && k1){
			//				tmp_sample_weight*=func_table[value].value();
			//			}
			//			if (value==0){
			//				tmp_assumptions.push(~Lit(var));
			//			}
			//			else{
			//				tmp_assumptions.push(Lit(var));
			//			}
			//
		}
		cout << "i mod interval " << i % interval << endl;
		//BE be(gm.variables,gm.functions,order);
		BESAT be(gm.variables, gm.clauses, order);
		double actual_weight = be.log_pe;//log(be.log_pe.toDouble().value());
		//tmp_exact_weight+=(exp(be.log_pe)/tmp_sample_weight);
		//cout<<actual_weight<<" [ "<<flush;
		//cout<<log(checkNumSolutions(gm,assumptions))<<" ]"<<flush;
		//double actual_weight=log(checkNumSolutions(gm,assumptions));
		//cout<<"("<<actual_weight<<" , "<<be.log_pe<<" ) "<<flush;
		//assert(fabs(actual_weight-be.log_pe) < 0.001);
		all_weights.push_back(actual_weight);
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}

		//Output z value after interval samples are generated
		if ((i % interval) == 0) {
			pair<double, double> tmp;
			tmp = purgeORStoreSAT(gm, helper, store, sampling_order, all_samples, all_weights, out);
			//weight_lb+=tmp.first;
			//weight_ub+=tmp.second;
			//weight+=(exp(actual_weight-sample_weight));
			Double pe_lb = Double((weight_lb + tmp.first) / (double) (i + 1));
			pe_lb *= gm.mult_factor;
			Double pe_ub = Double((weight_ub + tmp.second) / (double) (i + 1));
			pe_ub *= gm.mult_factor;
			out << "t " << getDateAndTime() << endl;
			out << "a " << pe_lb << " " << pe_ub << endl;
			//cout.setf(ios::fixed,ios::floatfield);
			//if(curr_time != some_time){
			//curr_time=time(NULL);
			//cout<<"z <"<<log10(pe_lb.value())<<","<< log10(pe_ub.value())<<" > "<<endl;
			cout << "z <" << pe_lb.value() << "," << pe_ub.value() << " > " << " , ";
			cout << "Storage space = " << printbytes(store.storage.size() * sizeof(ORStoreNode)) << endl;
			//cout<< "Exact z = "<<tmp_exact_weight/(double)i+1<<endl;
			if (tmp.first == tmp.second) {
				all_samples = vector<vector<int> > ();
				all_weights = vector<double> ();
				weight_lb += tmp.first;
				weight_ub += tmp.second;
				tmp.first = 0.0;
				tmp.second = 0.0;
				out << "p\n";
			}
			if ((int) store.storage.size() > 234217728) {
				all_samples = vector<vector<int> > ();
				all_weights = vector<double> ();
				weight_lb += tmp.first;
				weight_ub += tmp.second;
				store.reset();
				out << "p\n";
			}
		}
	}

	for (int j = 0; j < gm.variables.size(); j++) {
		gm.variables[j]->value() = INVALID_VALUE;
	}
	out.close();
	return Double(weight_ub) / Double((double) num_samples);
}

double RBSampleSearchSAT::runBE(GM& gm, vector<int>& order, char* infile)
{
	Solver solver;
	gzFile in = gzopen(infile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);

	//assumptions.push(Lit(0));
	BESAT be_sat(gm.variables, gm.clauses, order);
	cout << be_sat.log_pe << endl;
	return exp(be_sat.log_pe);
}

pair<double, double> purgeORStoreSATSampling(GM& gm, IJGPSamplingHelper& helper, ORStore& store, vector<int>& sampling_order, vector<vector<int> >& all_samples, vector<
		vector<bool> >& full_samples, vector<double>& all_weights, ofstream& out)
{

	assert(all_samples.size()==all_weights.size());
	assert(all_samples.size()==full_samples.size());
	//out<<"w "<<" ";
	double overall_weight_ub = 0.0;
	double overall_weight_lb = 0.0;
	for (int i = 0; i < all_samples.size(); i++) {
		double sample_weight_lb = 0.0, sample_weight_ub = 0.0;
		for (int j = 0; j < sampling_order.size(); j++) {
			gm.variables[sampling_order[j]]->value() = INVALID_VALUE;
		}
		assert(sampling_order.size() == all_samples[i].size());
		for (int j = 0; j < all_samples[i].size(); j++) {
			int var = sampling_order[j];
			int val = all_samples[i][j];
			vector<Double> func_table_lb(gm.variables[var]->domain_size());
			vector<Double> func_table_ub(gm.variables[var]->domain_size());
			Function& function = helper.getFunction(var);
			for (int k = 0; k < 2; k++) {
				func_table_lb[k] = Double();
				func_table_ub[k] = Double();
				if (store.check(var, k, false) != INCONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_ub[k] = function.table()[address];
				}
				if (store.check(var, k, false) == CONSISTENT) {
					gm.variables[var]->addr_value() = k;
					int address = Variable::getAddress(function.variables());
					func_table_lb[k] = function.table()[address];
				}
			}
			//Advance next_pointer
			store.check(var, val, true);
			gm.variables[var]->value() = val;
			//Normalize function table
			normalize_table(func_table_lb);
			normalize_table(func_table_ub);
			assert(!func_table_lb[val].isZero());
			assert(!func_table_ub[val].isZero());
			sample_weight_lb += log(func_table_lb[val].value());
			sample_weight_ub += log(func_table_ub[val].value());

		}
		//for(int k=0;k<gm.variables.size();k++){
		//out<<gm.variables[k]->value()<<" ";
		//}

		for (int k = 0; k < full_samples[i].size(); k++) {
			gm.variables[k]->value() = (full_samples[i][k]) ? (1) : (0);
		}
		for (int k = 0; k < gm.copy_of_variables.size(); k++) {
			out << gm.copy_of_variables[k]->value() << " ";
		}
		out << exp(all_weights[i] - sample_weight_lb) * gm.mult_factor.value() << " ";
		out << exp(all_weights[i] - sample_weight_ub) * gm.mult_factor.value() << endl;

		//cout<<all_weights[i]<<" "<<sample_weight<<" "<<exp(all_weights[i]-sample_weight)<<endl;
		overall_weight_lb += exp(all_weights[i] - sample_weight_lb);
		overall_weight_ub += exp(all_weights[i] - sample_weight_ub);
	}

	for (int j = 0; j < sampling_order.size(); j++) {
		gm.variables[sampling_order[j]]->value() = INVALID_VALUE;
	}
	//store.reset();
	return pair<double, double> (overall_weight_lb, overall_weight_ub);
}

void RBSampleSearchSAT::generateSamples(GM&gm, JG& jg, const char* satfile, int p_bound, vector<int>& order, vector<int>& sampling_order, int num_samples,
		int max_restarts)
{
	// Initialize the SAT solver
	Solver solver;
	gzFile in = gzopen(satfile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	ofstream out(outfilename.c_str());
	//out<<"t "<<getDateAndTime()<<endl;
	out << gm.copy_of_variables.size() << endl;
	vector<bool> is_sampled(order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		is_sampled[sampling_order[i]] = true;
	vector<int> helper_order;
	vector<int> helper_sampling_order;

	for (int i = 0; i < order.size(); i++)
		if (!is_sampled[order[i]])
			helper_order.push_back(order[i]);
		else
			helper_sampling_order.push_back(order[i]);
	//for(int i=(int)sampling_order.size()-1;i>-1;i--)
	//	helper_order.push_back(sampling_order[i]);
	for (int i = 0; i < helper_sampling_order.size(); i++)
		helper_order.push_back(helper_sampling_order[i]);
	sampling_order = vector<int> ();
	for (int i = helper_sampling_order.size() - 1; i > -1; i--)
		sampling_order.push_back(helper_sampling_order[i]);
	assert(helper_order.size()==order.size());
	IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order);
	myRandom random;
	double weight_lb = 0.0, weight_ub = 0.0;
	time_t curr_time;

	// Initialize the sampling order
	ORStore store(gm, sampling_order);
	vector<vector<int> > all_samples;
	vector<double> all_weights;
	vector<vector<bool> > full_samples;
	//double tmp_exact_weight=0.0;

	for (int i = 0; i < num_samples; i++) {
		// Set all variables to invalid value
		for (int j = 0; j < gm.variables.size(); j++)
			gm.variables[j]->value() = INVALID_VALUE;

		vec<Lit> assumptions;
		all_samples.push_back(vector<int> ());
		int cs = all_samples.size() - 1;
		double sample_weight = 0.0;
		double tmp_sample_weight = 1.0;
		assert(store.next_pointer == 0);
		vec<Lit> tmp_assumptions;
		set<Lit> implied_assignments;
		// Sample all variables according to the sampling order
		for (int j = 0; j < sampling_order.size(); j++) {
			//cerr<<" \t Processing variable "<<j<<" out of "<<sampling_order.size()<<endl;
			int var = sampling_order[j];
			// If the assignment is already implied move forward
			set<Lit>::iterator zero_lit, one_lit;
			zero_lit = implied_assignments.find(~Lit(var));
			one_lit = implied_assignments.find(Lit(var));
			assert(!(zero_lit!=implied_assignments.end() && one_lit!=implied_assignments.end()));
			if (zero_lit != implied_assignments.end()) {
				gm.variables[var]->value() = 0;
				store.AddNoGood(var, 1);
				store.AddAssignment(var, 0);
				assumptions.push(*zero_lit);
				continue;
			}
			else if (one_lit != implied_assignments.end()) {
				gm.variables[var]->value() = 1;
				store.AddNoGood(var, 0);
				store.AddAssignment(var, 1);
				assumptions.push(*one_lit);
				continue;
			}
			vector<Double> func_table(gm.variables[var]->domain_size());
			assert(gm.variables[var]->value()==INVALID_VALUE);
			// Initialize the proposal distribution for sampling
			Function& function = helper.getFunction(var);
			//vector<double> tmp_table(2);
			for (int k = 0; k < 2; k++) {
				gm.variables[var]->addr_value() = k;
				int address = Variable::getAddress(function.variables());
				func_table[k] = function.table()[address];
			}
			//for(int k=0;k<2;k++){
			//	tmp_table[k]=(func_table[k].value())/(func_table[0].value()+func_table[1].value());
			//}
			// Update the OR store
			set<Lit> imp_ass0, imp_ass1;
			if (store.check(var, 0, false) == DONTKNOW) {
				assumptions.push(~Lit(var));
				lbool solver_status = solver.solve(max_restarts, assumptions);
				if (solver_status == l_True) {
					store.AddGood(var, 0);
					imp_ass0 = solver.implied_assignments;
				}
				else if (solver_status == l_False) {
					store.AddNoGood(var, 0);
					store.AddGood(var, 1);
				}
				assumptions.pop();
			}
			if (store.check(var, 1, false) == DONTKNOW) {
				assumptions.push(Lit(var));
				lbool solver_status = solver.solve(max_restarts, assumptions);
				if (solver_status == l_True) {
					store.AddGood(var, 1);
					imp_ass1 = solver.implied_assignments;
				}
				else if (solver_status == l_False) {
					store.AddNoGood(var, 1);
					store.AddGood(var, 0);
				}
				assumptions.pop();
			}
			int value = -1;
			while (1) {
				value = generateSample(func_table, random, true);
				assert(value!=INVALID_VALUE);
				if (value == 0) {
					assumptions.push(~Lit(var));
				}
				else {
					assumptions.push(Lit(var));
				}
				if (isConsistentSAT(var, value, assumptions, store, solver)) {
					all_samples[cs].push_back(value);
					if (value == 0) {
						implied_assignments.insert(imp_ass0.begin(), imp_ass0.end());
					}
					else {
						implied_assignments.insert(imp_ass1.begin(), imp_ass1.end());
					}
					gm.variables[var]->value() = value;
					break;
				}
				else {
					func_table[value] = Double();
					assumptions.pop();
				}
			}

			// Extra code for checking
			//			bool k0=true,k1=true;
			//			for(int k=0;k<2;k++){
			//				if (k==0){
			//					tmp_assumptions.push(~Lit(var));
			//					if (!solver.solve(tmp_assumptions)){
			//						k0=false;
			//					}
			//				}
			//				else{
			//					tmp_assumptions.push(Lit(var));
			//					if (!solver.solve(tmp_assumptions)){
			//						k1=false;
			//					}
			//				}
			//				tmp_assumptions.pop();
			//			}
			//			assert(k0 || k1);
			//			if (k0 && k1){
			//				tmp_sample_weight*=func_table[value].value();
			//			}
			//			if (value==0){
			//				tmp_assumptions.push(~Lit(var));
			//			}
			//			else{
			//				tmp_assumptions.push(Lit(var));
			//			}
			//
		}

		for (set<Lit>::iterator j = implied_assignments.begin(); j != implied_assignments.end(); j++) {
			int curr_var = toInt(*j) >> 1;
			Lit p = Lit(curr_var);
			int curr_value = ((*j) == p) ? (1) : (0);
			gm.variables[curr_var]->value() = curr_value;

		}
		//BE be(gm.variables,gm.functions,order);
		BESAT be(gm.variables, gm.clauses, order);
		double actual_weight = be.log_pe;//log(be.log_pe.toDouble().value());
		//cout<<actual_weight<<endl;
		//tmp_exact_weight+=(exp(be.log_pe)/tmp_sample_weight);
		//cout<<actual_weight<<" [ "<<flush;
		//cout<<log(checkNumSolutions(gm,assumptions))<<" ]"<<flush;
		//double actual_weight=log(checkNumSolutions(gm,assumptions));
		//cout<<"("<<actual_weight<<" , "<<be.log_pe<<" ) "<<flush;
		//assert(fabs(actual_weight-be.log_pe) < 0.001);
		all_weights.push_back(actual_weight);

		BESampleSAT be_samplesat(gm.variables, gm.clauses, order, random);
		full_samples.push_back(vector<bool> ());
		int index = full_samples.size() - 1;
		vector<bool>& curr_full_sample = full_samples[index];
		for (int j = 0; j < gm.variables.size(); j++) {
			curr_full_sample.push_back(gm.variables[j]->value() == 1);
		}
		for (int j = 0; j < gm.variables.size(); j++) {
			gm.variables[j]->value() = INVALID_VALUE;
		}
		//Output z value after interval samples are generated
		if (i > 1 && i % interval == 0) {
			pair<double, double> tmp;
			tmp = purgeORStoreSATSampling(gm, helper, store, sampling_order, all_samples, full_samples, all_weights, out);
			//weight_lb+=tmp.first;
			//weight_ub+=tmp.second;
			//weight+=(exp(actual_weight-sample_weight));
			Double pe_lb = Double((weight_lb + tmp.first) / (double) i + 1);
			pe_lb *= gm.mult_factor;
			Double pe_ub = Double((weight_ub + tmp.second) / (double) i + 1);
			pe_ub *= gm.mult_factor;
			//out<<"t "<<getDateAndTime()<<endl;
			//out<<"a "<<pe_lb<<" "<<pe_ub<<endl;
			//cout.setf(ios::fixed,ios::floatfield);
			//if(curr_time != some_time){
			//curr_time=time(NULL);
			//cout<<"z <"<<log10(pe_lb.value())<<","<< log10(pe_ub.value())<<" > "<<endl;
			//cout<<"z <"<<pe_lb.value()<<","<< pe_ub.value()<<" > "<<" , ";
			cout << "Storage space = " << store.storage.size() * sizeof(ORStoreNode) << endl;
			//cout<<"Space = "<<store.storage.size()<<endl;
			//cout<< "Exact z = "<<tmp_exact_weight/(double)i+1<<endl;
			if (1) {
				all_samples = vector<vector<int> > ();
				all_weights = vector<double> ();
				weight_lb += tmp.first;
				weight_ub += tmp.second;
				full_samples.clear();
				//store.reset();
			}
			if ((store.storage.size() * sizeof(ORStoreNode)) > (unsigned int) 1073741824) {
				store.reset();
			}
		}
	}

	for (int j = 0; j < gm.variables.size(); j++) {
		gm.variables[j]->value() = INVALID_VALUE;
	}
	out.close();
	//return Double(weight_ub)/Double((double)num_samples);
}

void RBSampleSearchSAT::reduce(const char* satfilename, GM &gm, std::vector<vector<bool> >& new_domains)
{
	new_domains = vector<vector<bool> > (gm.variables.size());
	for (int i = 0; i < new_domains.size(); i++) {
		new_domains[i] = vector<bool> (gm.variables[i]->domain_size());
		for (int j = 0; j < new_domains[i].size(); j++) {
			new_domains[i][j] = false;
		}
	}
	Solver solver;
	gzFile in = gzopen(satfilename, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);

	solver.solve();
	for (int i = 0; i < solver.model.size(); i++) {
		int val = (solver.model[i] == l_True) ? (1) : (0);
		new_domains[i][val] = true;
	}
	cerr << "Solver inited\n";
	for (int i = 0; i < new_domains.size(); i++) {
		if (!new_domains[i][0]) {
			vec<Lit> assumptions;
			assumptions.push(~Lit(i));
			lbool solver_status = solver.solve(1, assumptions);
			if (solver_status != l_False)

			{
				new_domains[i][0] = true;
			}
		}
		if (!new_domains[i][1]) {
			vec<Lit> assumptions;
			assumptions.push(Lit(i));
			lbool solver_status = solver.solve(1, assumptions);
			if (solver_status != l_False)
			//if(solver.solve(assumptions))
			{
				new_domains[i][1] = true;
			}
		}
	}
	cerr << "Domains reduced\n";
}

Double getSampleSAT(vector<Function>& functions, Variable* variable, myRandom& random)
{
	assert(variable->value()==INVALID_VALUE);
	if (functions.empty()) {
		if (random.getDouble() < (double) 0.5) {
			variable->value() = 0;
		}
		else {
			variable->value() = 1;
		}
		return Double(0.5);
	}
	Function& function = functions[random.getInt(functions.size())];
	Double p1, p2;
	vector<Variable*> other_variables;
	for (int i = 0; i < function.variables().size(); i++) {
		if (function.variables()[i]->value() == INVALID_VALUE)
			other_variables.push_back(function.variables()[i]);
	}
	assert(!other_variables.empty());
	int num_values = Variable::getDomainSize(other_variables);
	for (int i = 0; i < num_values; i++) {
		Variable::setAddress(other_variables, i);
		int entry = Variable::getAddress(function.variables());
		if (variable->addr_value() == 0) {
			p1 += function.table()[entry];
		}
		else {
			p2 += function.table()[entry];
		}
	}
	long double p = (p1 / (p1 + p2)).value();
	if (random.getDouble() < p) {
		variable->value() = 0;
		return Double(p);
	}
	else {
		variable->value() = 1;
		return Double((long double) 1.0 - p);
	}
}
Double RBSampleSearchSAT::computePE(GM& gm, JG& jg, const char* satfile, int p_bound, vector<int>& order, vector<int>& sampling_order, int num_samples)
{
	Solver solver;
	gzFile in = gzopen(satfile, "rb");
	parse_DIMACS(in, solver);
	gzclose(in);
	ofstream out(outfilename.c_str());

	myRandom random;
	Double sum_weight;
	// First find the nodes
	vector<vector<Function> > var_to_functions(sampling_order.size());
	vector<int> var2index(gm.variables.size());
	for (int i = 0; i < var2index.size(); i++) {
		var2index[i] = INVALID_VALUE;
	}
	for (int i = 0; i < sampling_order.size(); i++) {
		var2index[sampling_order[i]] = i;
	}

	for (int i = 0; i < jg.nodes.size(); i++) {
		for (int j = 0; j < jg.nodes[i]->variables().size(); j++) {
			if (var2index[jg.nodes[i]->variables()[j]->id()] != INVALID_VALUE) {
				int index = var2index[jg.nodes[i]->variables()[j]->id()];
				var_to_functions[index].push_back(Function());
				jg.nodes[i]->getMarginal(jg.nodes[i]->variables(), var_to_functions[index][var_to_functions[index].size() - 1]);
			}
		}
	}
	time_t old_time = time(NULL);
	for (int i = 0; i < num_samples; i++) {
		vec<Lit> assumptions;
		set<Lit> implied_assignments;
		for (int j = 0; j < gm.variables.size(); j++)
			gm.variables[j]->value() = INVALID_VALUE;
		long double sample_weight = 0.0;
		for (int j = 0; j < sampling_order.size(); j++) {
			int var = sampling_order[j];
			// If the assignment is already implied move forward
			set<Lit>::iterator zero_lit, one_lit;
			zero_lit = implied_assignments.find(~Lit(var));
			one_lit = implied_assignments.find(Lit(var));
			assert(!(zero_lit!=implied_assignments.end() && one_lit!=implied_assignments.end()));
			if (zero_lit != implied_assignments.end()) {
				gm.variables[var]->value() = 0;
				assumptions.push(*zero_lit);
				continue;
			}
			else if (one_lit != implied_assignments.end()) {
				gm.variables[var]->value() = 1;
				assumptions.push(*one_lit);
				continue;
			}
			set<Lit> imp_ass0, imp_ass1;
			assumptions.push(~Lit(var));
			if (solver.solve(assumptions)) {
				imp_ass0 = solver.implied_assignments;
				assumptions.pop();
				assumptions.push(Lit(var));
				if (solver.solve(assumptions)) {
					imp_ass1 = solver.implied_assignments;
					assumptions.pop();
					sample_weight += log(getSampleSAT(var_to_functions[j], gm.variables[var], random).value());
					if (gm.variables[var]->value() == 0) {
						assumptions.push(~Lit(var));
						implied_assignments.insert(imp_ass0.begin(), imp_ass0.end());
					}
					else {
						assumptions.push(Lit(var));
						implied_assignments.insert(imp_ass1.begin(), imp_ass1.end());
					}
				}
				else {
					assumptions.pop();
					implied_assignments.insert(imp_ass0.begin(), imp_ass0.end());
					assumptions.push(~Lit(var));
					gm.variables[var]->value() = 0;
					continue;
				}
			}
			else {
				gm.variables[var]->value() = 1;
				assumptions.pop();
				assumptions.push(Lit(var));
			}
		}
		for (set<Lit>::iterator j = implied_assignments.begin(); j != implied_assignments.end(); j++) {
			int curr_var = toInt(*j) >> 1;
			Lit p = Lit(curr_var);
			int curr_value = ((*j) == p) ? (1) : (0);
			gm.variables[curr_var]->value() = curr_value;

		}
		BESAT be(gm.variables, gm.clauses, order);
		cerr << exp(be.log_pe) << " " << exp(sample_weight) << " ";
		cerr << gm.mult_factor.value() << endl;
		sum_weight += Double(exp(be.log_pe - sample_weight));
		time_t curr_time = time(NULL);
		if (curr_time > (old_time + 60)) {
			old_time = curr_time;
			cout << "z " << log10((sum_weight / Double((long double) i)).value()) << endl;
		}
	}
}
