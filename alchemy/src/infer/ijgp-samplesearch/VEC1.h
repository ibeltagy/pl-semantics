/*
 * VEC1.h
 *
 *  Created on: Jun 29, 2010
 *      Author: Vibhav Gogate
 *      Email: vgogate@cs.washington.edu
 *      University of Washington, Seattle
 *      All rights reserved.
 */

#ifndef SS_VEC_1_H_
#define SS_VEC_1_H_

namespace ss{

extern void setupSolver(Solver& S, GM& gm, bool reduce = false);
struct VEC_NODE
{
private:
	Variable* v;
	vector<bool> domain;
	int n;
public:
	VEC_NODE() :
		v(NULL), domain(vector<bool> ()), n(-1)
	{
	}
	VEC_NODE(Variable* v_) :
		v(v_)
	{
		v->value() = INVALID_VALUE;
		domain = vector<bool> (v->domain_size());
		n = domain.size();
		for (int i = 0; i < domain.size(); i++) {
			domain[i] = true;
		}
	}
	void removeValue(int j)
	{
		assert((int) j < domain.size());
		if (domain[j]) {
			n--;
			domain[j] = false;
		}
	}
	void reset()
	{
		v->value() = INVALID_VALUE;
		domain = vector<bool> (v->domain_size());
		n = domain.size();
		for (int i = 0; i < domain.size(); i++) {
			domain[i] = true;
		}
	}
	int getN()
	{
		return n;
	}
	bool setNextValue()
	{
		int j = v->value();
		if (j ==INVALID_VALUE)
			j=v->domain_size();
		while (1) {
			j--;
			//if ((int) j == domain.size())
			if (j<0)
				return false;
			if (domain[j]) {
				v->value() = j;
				return true;
			}
		}
		return false;
	}
};
void print_assignment(vector<Variable*>& vars)
{
	for (int i = 0; i < vars.size(); i++) {
		cout << vars[i]->id() << "=" << vars[i]->value() << ",";
	}
	cout << endl;
}
void VEC_DET(GM& gm, vector<int>& order, vector<int>& sampling_order, clock_t& time_bound, ostream& out)
{
cout << "in vec det" << endl;
	assert(gm.mode == DET);
	if (sampling_order.empty()) {
		BE be(gm.variables, gm.functions, order);
		cerr << "Exact result\n";
		out << (gm.mult_factor.value() * exp(be.log_pe.toLongdouble())) << endl;
		cerr << (gm.mult_factor.value() * exp(be.log_pe.toLongdouble())) << endl;
		return;
	}
	Timer timer;
	timer.start();
	vector<Variable*> cond_variables(sampling_order.size());
	for (int i = 0; i < sampling_order.size(); i++) {
		cond_variables[i] = gm.variables[sampling_order[i]];
		cond_variables[i]->value() = INVALID_VALUE;
	}
	vector<long double> log10_numvalues(cond_variables.size());
	long double log10_numvalue = 0;
	for (int i = cond_variables.size() - 1; i > -1; i--) {
		log10_numvalues[i] = log10_numvalue;
		log10_numvalue += log10((double) cond_variables[i]->domain_size());
	}
	//int num_values = Variable::getDomainSize(cond_variables);
	//Double pe;
	long double pe = 0.0;
	cerr << "log 10 num values = " << log10_numvalue << endl;

	//for (int i = 0; i < gm.functions.size() ; i ++)
	//	gm.functions[i]->print();

	Solver solver;
	gm.convertToSATUAI10();
	setupSolver(solver, gm, true);
	vector<VEC_NODE> all_doms(cond_variables.size());
	vec<Lit> assumptions;
	for (int i = 0; i < cond_variables.size(); i++) {
		all_doms[i] = VEC_NODE(cond_variables[i]);
	}
	for (int j = 0; j < cond_variables[0]->domain_size(); j++) {
		int id = cond_variables[0]->id();
		assumptions.push(Lit(gm.csp_to_sat_variables[id][j]));
		if (!solver.solve(assumptions)) {
			all_doms[0].removeValue(j);
		}
		assumptions.pop();
	}
	int i = 0;

	bool has_timed_out = false;
	long double num_solutions = 0.0;
	long double num_explored = 0.0;
	while (i >= 0) {
		if (timer.timeout(time_bound)) {
			has_timed_out = true;
			break;
		}
		// Forward phase
		if (all_doms[i].setNextValue()) {
			int id = cond_variables[i]->id();
			int value = cond_variables[i]->value();
			assumptions.push(Lit(gm.csp_to_sat_variables[id][value]));
			i++;
			// BE phase
			if (i == (int) cond_variables.size()) {
				//print_assignment(cond_variables);
				// Run Bucket elimination
				BE be(gm.variables, gm.functions, order);
				//pe += exp(be.log_pe.toLongdouble());
				num_explored += 1.0;
				long double curr_num_solutions = 1.0;
				for (int j = 0; j < all_doms.size(); j++) {
					curr_num_solutions *= all_doms[j].getN();
				}

				pe += exp(be.log_pe.toLongdouble());
				num_solutions += curr_num_solutions;
				assumptions.pop();
				i--;
			}
			else {
				all_doms[i].reset();
				for (int j = 0; j < cond_variables[i]->domain_size(); j++) {
					int id = cond_variables[i]->id();
					assumptions.push(Lit(gm.csp_to_sat_variables[id][j]));
					if (!solver.solve(assumptions)) {
						all_doms[i].removeValue(j);
					}
					assumptions.pop();
				}
			}
		}
		//Backward phase
		else {
			assumptions.pop();
			i--;
		}
	}

	if (has_timed_out) {
		long double log10_explored = 0.0;
		for (int i = 0; i < cond_variables.size(); i++) {
			if (cond_variables[i]->value()==INVALID_VALUE)
				continue;
			int curr_val=cond_variables[i]->domain_size()-cond_variables[i]->value()-1;
			if (curr_val<=0) {
				continue;
			}
			log10_explored = log10_numvalues[i] + log10(curr_val);
			break;
		}
		cerr << "Explored = " << log10_explored << "out of " << log10_numvalue << endl;
		long double log10_factor = log10_numvalue - log10_explored;
		//out << log10(pe * gm.mult_factor.value()) + log10_factor << endl;
		out << pe * gm.mult_factor.value() * pow(10, log10_factor) << endl;
		cerr << log10(pe * gm.mult_factor.value()) + log10_factor << endl;
	}
	else {
		out << (pe * gm.mult_factor.value()) << endl;
		cerr << (pe * gm.mult_factor.value()) << endl;
		cerr << "Exact answer\n";
	}
	for (int i = 0; i < cond_variables.size(); i++)
		cond_variables[i]->value() = INVALID_VALUE;
}

void VEC(GM& gm, vector<int>& order, vector<int>& sampling_order, clock_t& time_bound, ostream& out)
{
	Timer timer;
	timer.start();
	vector<Variable*> cond_variables(sampling_order.size());
	for (int i = 0; i < sampling_order.size(); i++)
		cond_variables[i] = gm.variables[sampling_order[i]];
	long double log10_num_values = 0;
	for (int i = 0; i < cond_variables.size(); i++) {
		log10_num_values += log10((double) cond_variables[i]->domain_size());
	}
	//int num_values = Variable::getDomainSize(cond_variables);
	//Double pe;
	long double pe = 0.0;
	cerr << "log 10 num values = " << log10_num_values << endl;
	Solver solver;

	if (gm.mode == DET) {
		gm.convertToSATUAI10();
		setupSolver(solver, gm, true);
	}
	int n = cond_variables.size();
	int a[n];
	int f[n + 1];
	int o[n];
	int m[n];
	// Initialize
	for (int i = 0; i < n; i++) {
		cond_variables[i]->value() = 0;
		a[i] = 0;
		f[i] = i;
		o[i] = 1;
		m[i] = cond_variables[i]->domain_size();
	}
	f[n] = n;
	bool has_timed_out = false;
	long double num_explored = 0.0;
	bool first_iteration_over = false;
	while (1) {
		bool has_sol = true;
		if (timer.timeout(time_bound) && first_iteration_over) {
			has_timed_out = true;
			break;
		}
		first_iteration_over = true;
		num_explored += (1.0);
		if (gm.mode == DET) {
			vec<Lit> assumptions;
			for (int j = 0; j < cond_variables.size(); j++) {
				int id = cond_variables[j]->id();
				int value = cond_variables[j]->value();
				assumptions.push(Lit(gm.csp_to_sat_variables[id][value]));
			}
			if (!solver.solve(assumptions)) {
				has_sol = false;
			}
		}
		if (has_sol) {

			BE be(gm.variables, gm.functions, order);
			//pe+=be.log_pe.toDouble();
			pe += exp(be.log_pe.toLongdouble());
		}
		int j = f[0];
		f[0] = 0;
		if (j == n)
			break;
		a[j] = a[j] + o[j];
		cond_variables[j]->value() = a[j];
		if (a[j] == 0 || a[j] == (m[j] - 1)) {
			o[j] = -o[j];
			f[j] = f[j + 1];
			f[j + 1] = j + 1;
		}
	}
	//cout << "Num-explored = " << num_explored << endl;
	cerr << "Explored = " << num_explored << "out of " << pow(10,log10_num_values) << endl;
	if (has_timed_out) {
		long double log10_factor = log10_num_values - log10(num_explored);
		//out << log10(pe * gm.mult_factor.value()) + log10_factor << endl;
		out << pe * gm.mult_factor.value() * pow(10, log10_factor)  << endl;

	}
	else {
		out << pe * gm.mult_factor.value() << endl;
		cerr << "Exact answer\n";
	}
}
void VEC_MAR(GM& gm, vector<int>& order, vector<int>& sampling_order, vector<set<int> >& clusters, clock_t& time_bound, ostream& out)
{
	Timer timer;
	timer.start();
	vector<Variable*> cond_variables(sampling_order.size());
	vector<bool> is_sampled(gm.variables.size());
	for (int i = 0; i < sampling_order.size(); i++) {
		cond_variables[i] = gm.variables[sampling_order[i]];
		is_sampled[cond_variables[i]->id()] = true;
	}
	long double log10_num_values = 0;
	for (int i = 0; i < cond_variables.size(); i++) {
		log10_num_values += log10((double) cond_variables[i]->domain_size());
	}
	vector<vector<Double> > marginals(gm.variables.size());
	for (int i = 0; i < marginals.size(); i++) {
		marginals[i] = vector<Double> (gm.variables[i]->domain_size());
	}
	long double pe = 0.0;
	cerr << "log 10 num values = " << log10_num_values << endl;
	Solver solver;
	JT jt(gm.variables, gm.functions, clusters, order);
	if (gm.mode == DET) {
		gm.convertToSATUAI10();
		setupSolver(solver, gm, true);
	}
	int n = cond_variables.size();
	int a[n];
	int f[n + 1];
	int o[n];
	int m[n];
	// Initialize
	for (int i = 0; i < n; i++) {
		cond_variables[i]->value() = 0;
		a[i] = 0;
		f[i] = i;
		o[i] = 1;
		m[i] = cond_variables[i]->domain_size();
	}
	f[n] = n;
	bool has_timed_out = false;
	long double num_explored = 0.0;
	bool first = true;
	long double num_consistent = 0;
	while (1) {
		bool has_sol = true;
		if (timer.timeout(time_bound)) {
			has_timed_out = true;
			break;
		}
		num_explored += (1.0);
		if (gm.mode == DET) {
			vec<Lit> assumptions;
			for (int j = 0; j < cond_variables.size(); j++) {
				int id = cond_variables[j]->id();
				int value = cond_variables[j]->value();
				assumptions.push(Lit(gm.csp_to_sat_variables[id][value]));
			}
			if (!solver.solve(assumptions)) {
				has_sol = false;
			}
		}
		if (has_sol) {
			num_consistent += 1.0;
			BE be(gm.variables, gm.functions, order);
			//pe+=be.log_pe.toDouble();
			pe += exp(be.log_pe.toLongdouble());
			if (num_consistent < (long double) 10 || (log(pe) - be.log_pe.toLongdouble() - log(num_consistent)) < (long double)13) {
				cout << exp(be.log_pe.toLongdouble()) << endl;
				jt.propagate();

				/*
				 BucketProp jt(gm.variables, gm.functions, order);
				 pe += exp(jt.log_pe.toLongdouble());
				 */

				for (int ii = 0; ii < jt.marginals.size(); ii++) {
					if (!is_sampled[ii]) {
						for (int jj = 0; jj < jt.marginals[ii].tableSize(); jj++) {
							marginals[ii][jj] += jt.marginals[ii].tableEntry(jj) * Double(exp(be.log_pe.toLongdouble()));
							//marginals[ii][jj] += jt.marginals[ii].table()[jj] * Double(exp(jt.log_pe.toLongdouble()));
						}
					}
					else {
						marginals[ii][gm.variables[ii]->value()] += Double(exp(be.log_pe.toLongdouble()));
						//marginals[ii][gm.variables[ii]->value()] += Double(exp(jt.log_pe.toLongdouble()));
					}
				}
			}
			else {
				cerr << "not propagating: "<<log(pe)-log(num_consistent)<<" "<<be.log_pe.toLongdouble()<<endl;
			}
		}
		int j = f[0];
		f[0] = 0;
		if (j == n)
			break;
		a[j] = a[j] + o[j];
		cond_variables[j]->value() = a[j];
		if (a[j] == 0 || a[j] == (m[j] - 1)) {
			o[j] = -o[j];
			f[j] = f[j + 1];
			f[j + 1] = j + 1;
		}
	}

	//cout << "num-explored = " << num_explored << endl;
	cerr << "Explored = " << num_explored << "out of " << pow(10,log10_num_values) << endl;
	for (int i = 0; i < cond_variables.size(); i++)
		cond_variables[i]->value() = INVALID_VALUE;
	if (has_timed_out) {
		cout << "PE = " << log10(pe * gm.mult_factor.value()) << endl;
		gm.printMarginalsUAI10(marginals, out);
	}
	else {
		cout << "PE = " << log10(pe * gm.mult_factor.value()) << endl;
		gm.printMarginalsUAI10(marginals, out);
		cerr << "Exact answer\n";
	}
}
void VEC_MAR_DET(GM& gm, vector<int>& order, vector<int>& sampling_order, vector<set<int> >& clusters, clock_t& time_bound, ostream& out)
{
	assert(gm.mode == DET);
	if (sampling_order.empty()) {
		JT jt(gm.variables, gm.functions, clusters, order);
		jt.propagate();
		gm.printMarginalsUAI10(jt.marginals, out);
		cerr << "Exact answer\n";
		return;
	}
	Timer timer;
	timer.start();
	vector<Variable*> cond_variables(sampling_order.size());
	vector<bool> is_sampled(gm.variables.size());
	for (int i = 0; i < sampling_order.size(); i++) {
		cond_variables[i] = gm.variables[sampling_order[i]];
		is_sampled[cond_variables[i]->id()] = true;
	}
	long double log10_num_values = 0;
	for (int i = 0; i < cond_variables.size(); i++) {
		log10_num_values += log10((double) cond_variables[i]->domain_size());
	}
	vector<vector<Double> > marginals(gm.variables.size());
	for (int i = 0; i < marginals.size(); i++) {
		marginals[i] = vector<Double> (gm.variables[i]->domain_size());
	}
	long double pe = 0.0;
	cerr << "log 10 num values = " << log10_num_values << endl;
	Solver solver;
	JT jt(gm.variables, gm.functions, clusters, order);
	gm.convertToSATUAI10();
	setupSolver(solver, gm, true);
	vector<VEC_NODE> all_doms(cond_variables.size());
	for (int i = 0; i < cond_variables.size(); i++) {
		all_doms[i] = VEC_NODE(cond_variables[i]);
	}
	int i = 0;
	vec<Lit> assumptions;
	bool has_timed_out = false;
	long double num_solutions = 0.0;
	long double num_explored = 0.0;
	while (i >= 0) {
		if (timer.timeout(time_bound)) {
			has_timed_out = true;
			break;
		}
		// Forward phase
		if (all_doms[i].setNextValue()) {
			int id = cond_variables[i]->id();
			int value = cond_variables[i]->value();
			assumptions.push(Lit(gm.csp_to_sat_variables[id][value]));
			i++;
			// BE phase
			if (i == (int) cond_variables.size()) {
				BE be(gm.variables, gm.functions, order);
				//pe+=be.log_pe.toDouble();
				pe += exp(be.log_pe.toLongdouble());

				jt.propagate();

				/*
				 BucketProp jt(gm.variables, gm.functions, order);
				 pe += exp(jt.log_pe.toLongdouble());
				 */

				for (int ii = 0; ii < jt.marginals.size(); ii++) {
					if (!is_sampled[ii]) {
						for (int jj = 0; jj < jt.marginals[ii].tableSize(); jj++) {
							marginals[ii][jj] += jt.marginals[ii].tableEntry(jj) * Double(exp(be.log_pe.toLongdouble()));
							//marginals[ii][jj] += jt.marginals[ii].table()[jj] * Double(exp(jt.log_pe.toLongdouble()));
						}
					}
					else {
						marginals[ii][gm.variables[ii]->value()] += Double(exp(be.log_pe.toLongdouble()));
						//marginals[ii][gm.variables[ii]->value()] += Double(exp(jt.log_pe.toLongdouble()));
					}
				}
				//print_assignment(cond_variables);
				// Run Bucket elimination

				num_explored += 1.0;
				long double curr_num_solutions = 1.0;
				for (int j = 0; j < all_doms.size(); j++) {
					curr_num_solutions *= all_doms[j].getN();
				}
				num_solutions += curr_num_solutions;
				//cout << "Explored solution " << num_explored << " out of " << num_solutions / num_explored << endl;
				assumptions.pop();
				i--;
			}
			else {
				all_doms[i].reset();
				for (int j = 0; j < cond_variables[i]->domain_size(); j++) {
					int id = cond_variables[i]->id();
					assumptions.push(Lit(gm.csp_to_sat_variables[id][j]));
					if (!solver.solve(assumptions)) {
						all_doms[i].removeValue(j);
					}
					assumptions.pop();
				}
			}
		}
		//Backward phase
		else {
			assumptions.pop();
			i--;
		}
	}

	cerr << "Explored = " << num_explored << "out of " << pow(10,log10_num_values) << endl;
	for (int i = 0; i < cond_variables.size(); i++)
		cond_variables[i]->value() = INVALID_VALUE;
	if (has_timed_out) {
		cout << "PE = " << log10(pe * gm.mult_factor.value()) << endl;
		gm.printMarginalsUAI10(marginals, out);
	}
	else {
		cout << "PE = " << log10(pe * gm.mult_factor.value()) << endl;
		gm.printMarginalsUAI10(marginals, out);
		cerr << "Exact answer\n";
	}
}
}
#endif /* VEC_H_ */
