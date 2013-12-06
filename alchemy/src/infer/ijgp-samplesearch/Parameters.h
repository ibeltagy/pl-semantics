/*
 * Parameters.h
 *
 *  Created on: Jun 28, 2010
 *      Author: Vibhav Gogate
 *      Email: vgogate@cs.washington.edu
 *      University of Washington, Seattle
 *      All rights reserved.
 */

#ifndef SS_PARAMETERS_H_
#define SSS_PARAMETERS_H_

namespace ss{

struct UAI2010Parameters
{
	// The graphical model
	GM& gm;
	// Time information
	clock_t& time_bound;
	//clock_t& memory_bound;

	// Task type
	string& task;

	// The best treewidth order
	vector<int> bt_order;
	// The best w-cutset order
	vector<int> bw_order;
	// The sampling order
	vector<int> s_order;
	vector<set<int> > bt_clusters;
	vector<set<int> > bw_clusters;
	int num_iterations;
	// i-bound, rb-bound and max-restarts
	int i_bound;
	double rb_bound;
	int max_restarts;
	int max_domain_size;
	static int I_BOUND_;
	static int RB_BOUND_;
	static int NUM_ITERATIONS_;
	void adjust_i_bound()
	{
		i_bound = (int) floor(log(2) * (double) i_bound / log(max_domain_size));
	}
	UAI2010Parameters(GM& gm_, clock_t& curr_time_, string& task_) :
		gm(gm_), time_bound(curr_time_), task(task_), max_restarts(1), num_iterations(10), i_bound(1), rb_bound(1),max_domain_size(0)
	{
		double estimate;
		max_domain_size = 0;
		int eff_var_size = 0;
		for (int i = 0; i < gm.variables.size(); i++) {
			if (max_domain_size < (int) gm.variables[i]->domain_size())
				max_domain_size = gm.variables[i]->domain_size();
			if (gm.variables[i]->value() == INVALID_VALUE) {
				eff_var_size++;
			}
		}
		if ((int) time_bound < (int) 50) {
			if (I_BOUND_<0){
				I_BOUND_=10;
			}
			if (RB_BOUND_<0){
				RB_BOUND_=15;
			}
			if (NUM_ITERATIONS_<0){
				NUM_ITERATIONS_=10;
			}
			i_bound = I_BOUND_;
			adjust_i_bound();
			rb_bound = log((double) (1 << RB_BOUND_)) - log(eff_var_size);
			if (i_bound <0){
				i_bound=1;
			}
			if (rb_bound < 0){
				rb_bound=0;
			}
			num_iterations=NUM_ITERATIONS_;
			cout << "i-bound = " << i_bound << endl;
			cout << "rb-bound = " << rb_bound << endl;
			cout << "num iterations  = "<<num_iterations<<endl;
			gm.getMinFillOrdering(bt_order, bt_clusters, estimate);
			bw_order = bt_order;
			bw_clusters = bt_clusters;
			// set i-bound
			gm.rearrangeOrdering(bw_order, bw_clusters, s_order, rb_bound);
			cerr << "# sampled = " << s_order.size() << endl;
		}
		else {
			if (NUM_ITERATIONS_<0){
				NUM_ITERATIONS_=17;
			}
			if (I_BOUND_ < 0) {
				I_BOUND_ = 14;
			}
			if (RB_BOUND_ < 0) {
				RB_BOUND_ = 23;
			}
			i_bound = I_BOUND_;
			adjust_i_bound();
			rb_bound = log((double) (1 << RB_BOUND_)) - log(eff_var_size);
			if (i_bound < 0) {
				i_bound = 1;
			}
			if (rb_bound < 0) {
				rb_bound = 0;
			}
			num_iterations=NUM_ITERATIONS_;
			cout << "i-bound = " << i_bound << endl;
			cout << "rb-bound = " << rb_bound << endl;
			cout << "num iterations  = "<<num_iterations<<endl;
			int smallest_max_cluster_size = gm.variables.size();
			vector<int> curr_order, curr_s_order;
			vector<set<int> > curr_clusters;
			int curr_max_cluster_size;
			double curr_cutset_size;
			double smallest_cutset_size=DBL_MAX;
			cout << "Smallest cutset size inited to " << smallest_cutset_size << endl;
			Timer timer;
			timer.start();
			clock_t ord_time_bound=30;
			for (int i = 0; i < 50000; i++) {
				curr_order.clear();
				curr_clusters.clear();
				if (timer.timeout(ord_time_bound)) {
					break;
				}
				gm.getMinFillOrdering_randomized(curr_order, curr_clusters, estimate, curr_max_cluster_size);
				//cout<<curr_max_cluster_size<<endl;
				if (curr_max_cluster_size < smallest_max_cluster_size) {
					smallest_max_cluster_size = curr_max_cluster_size;
					bt_clusters = curr_clusters;
					bt_order = curr_order;
				}
				for (int j = 0; j < 2; j++) {
					gm.rearrangeOrdering_randomized(curr_order, curr_clusters, curr_s_order, rb_bound);
					//gm.rearrangeOrdering(curr_order, curr_clusters, curr_s_order, rb_bound);

					curr_cutset_size = 0;
					for (int k = 0; k < curr_s_order.size(); k++) {
						curr_cutset_size += log(gm.variables[curr_s_order[k]]->domain_size());
					}
					if (curr_cutset_size < smallest_cutset_size) {
						cout << curr_cutset_size << " " << smallest_cutset_size << " " << i << " " << j << endl;
						smallest_cutset_size = curr_cutset_size;
						bw_order = curr_order;
						bw_clusters = curr_clusters;
						s_order = curr_s_order;
					}
					if (timer.timeout(ord_time_bound)) {
						break;
					}
				}
			}
			for (int i = 0; i < 1; i++) {
				gm.getLexOrdering(curr_order, curr_clusters, estimate, curr_max_cluster_size, smallest_max_cluster_size);
				cout << curr_max_cluster_size << endl;
				if (curr_max_cluster_size < smallest_max_cluster_size) {
					smallest_max_cluster_size = curr_max_cluster_size;
					bt_clusters = curr_clusters;
					bt_order = curr_order;

					for (int j = 0; j < 1; j++) {
						//gm.rearrangeOrdering_randomized(curr_order, curr_clusters, curr_s_order, rb_bound);
						gm.rearrangeOrdering(curr_order, curr_clusters, curr_s_order, rb_bound);
						curr_cutset_size = 0;
						for (int k = 0; k < curr_s_order.size(); k++) {
							curr_cutset_size += log(gm.variables[curr_s_order[k]]->domain_size());
						}
						cout << "Lex: #sampled = " << curr_s_order.size() << endl;
						if (curr_cutset_size < smallest_cutset_size) {
							cout << curr_cutset_size << " " << smallest_cutset_size << endl;
							smallest_cutset_size = curr_cutset_size;
							bw_order = curr_order;
							bw_clusters = curr_clusters;
							s_order = curr_s_order;
						}
					}
				}
			}
			cerr << "Treewidth = " << smallest_max_cluster_size << endl;
			cerr << "# sampled = " << s_order.size() << endl;
		}
	}
	bool exact_inf_test()
	{

		double how_many = 0.0;
		double limit;
		if (task == "PR" || max_domain_size > 2) {
			limit = 3.2;
		}
		else {
			limit = 4.5;
		}

		if ((int)time_bound < 50){
			if (max_domain_size > 2){
				limit=1.2;
			}
		}
		cerr<<"Limit for exact inf = "<<limit<<endl;
		for (int i = 0; i < s_order.size(); i++) {
			int var = s_order[i];
			how_many += log10(gm.variables[var]->domain_size());
			/*
			 if (gm.mode==DET){
			 if (how_many > (double) 20){
			 return false;
			 }
			 }
			 else{
			 */
			if (how_many > limit) {
				return false;
			}
			//}
		}
		return true;
	}
};
}
#endif /* PARAMETERS_H_ */
