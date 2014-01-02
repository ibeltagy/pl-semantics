#ifndef SS_RB_IJGP_SAMPLER_H_
#define SS_RB_IJGP_SAMPLER_H_

#include "GM.h"
#include "JG.h"
#include "IJGPSamplingHelper.h"
#include "myRandom.h"
#include "BE.h"
#include "JT.h"
#include "Timer.h"

namespace ss{

class RB_IJGP_Sampler {
private:
	void updateWeight(GM& gm, vector<int>& sampled_variables,
			vector<int>& other_variables, Double& weight,
			vector<Function>& marginals_) {
		for (int i = 0; i < sampled_variables.size(); i++) {
			int id = sampled_variables[i];
			marginals[id][gm.variables[id]->value()] += weight;
		}
		for (int i = 0; i < other_variables.size(); i++) {
			int id = other_variables[i];
			//for(int j=0;j<gm.variables[id]->domain_size();j++)
			//{
			//marginals[id][j]+=marginals_[id].table()[j];
			//}
			marginals[id][gm.variables[id]->value()] += weight;
		}
	}
	void updateWeight(GM& gm, Double& weight) {
		for (int i = 0; i < gm.variables.size(); i++) {
			int id = gm.variables[i]->id();
			marginals[id][gm.variables[id]->value()] += weight;
		}
	}
public:
	vector<vector<Double> > marginals;
	RB_IJGP_Sampler() {
	}
	string getDateAndTime() {
		time_t p = time(NULL);
		char *s = ctime(&p);
		s[strlen(s) - 1] = 0;
		string out(s);
		return out;
	}

	//void computeBeliefs(GM& gm, JG& jg, int p_bound, vector<int>& order, int time_limit,  int num_samples,vector<vector<Double> >& beliefs)
	//{
	//	IJGPSamplingHelper helper(&gm,&jg,p_bound,order);
	//	Double weight;
	//	myRandom random;
	//	time_t start_time,curr_time;
	//	start_time=time(NULL);
	//	beliefs=vector<vector<Double> > (gm.variables.size());
	//	for(int i=0;i<gm.variables.size();i++)
	//	{
	//		beliefs[i]=vector<Double> (gm.variables[i]->domain_size());
	//	}
	//
	//	for(int samples=0;samples<num_samples;samples++)
	//	{
	//		if(samples % 1000 ==0)
	//			cerr<<"Sample "<<samples<<" taken\n";
	//		curr_time=time(NULL);
	//		if(num_samples==INVALID_VALUE && (curr_time-start_time) >= time_limit)
	//		{
	//		
	//			return;
	//		}
	//		// Initialize  ;'v;vp-pkvv;
	//		for(int i=0;i<gm.variables.size();i++)
	//		{
	//			gm.variables[i]->value()=INVALID_VALUE;
	//		}
	//		double sample_weight=0.0;
	//		double actual_weight=0.0;
	//		for(int i=0;i<gm.variables.size();i++)
	//		{
	//			int var=order[order.size()-1-i];
	//			Double curr_weight;
	//			int value;
	//			helper.getSample(var,value,curr_weight,random);
	//			gm.variables[var]->value()=value;
	//			//cout<<i<<endl;
	//			//cout<<curr_weight<<" ";
	//			//if(curr_weight.isZero())
	//			//{
	//			//	cerr<<"Weight is zero\n";
	//			//	exit(1);
	//			//}
	//			
	//			sample_weight+=log(curr_weight.value());
	//		}
	//		//cout<<endl;
	//		//cerr<<"Sample Weight = "<<sample_weight<<endl;
	//		for(int i=0;i<gm.functions.size();i++)
	//		{
	//			Double tmp=gm.functions[i]->getWeight();
	//			//if(tmp.isZero())
	//			//{
	//
	//			//	cerr<<"Actual Weight is zero\n";
	//			//	exit(1);
	//			//}
	//			actual_weight+=log(tmp.value());
	//		}
	//		Double curr_weight(exp(actual_weight-sample_weight));
	//		for(int i=0;i<gm.variables.size();i++)
	//		{
	//			assert(gm.variables[i]->value()!=INVALID_VALUE);
	//			beliefs[i][gm.variables[i]->value()]+=curr_weight;	
	//		}	
	//		//cerr<<"Actual weight = "<<actual_weight<<endl;
	//		weight+=(exp(actual_weight-sample_weight));
	//	}
	//	for(int i=0;i<gm.variables.size();i++)
	//	{
	//		gm.variables[i]->value()=INVALID_VALUE;
	//	}
	//	for(int i=0;i<gm.variables.size();i++)
	//	{
	//		gm.variables[i]->value()=INVALID_VALUE;
	//		for(int j=0;j<beliefs[i].size();j++)
	//			beliefs[i][j]/=weight;
	//	}
	//	return;
	//}
	long double computePE(GM& gm, JG& jg, int p_bound, vector<int>& order,
			vector<int>& sampling_order, clock_t& time_bound) {
		Timer timer;
		timer.start();
		SAMPLER_TYPE type = ZERO_SAMPLER;
		if (gm.mode == POSITIVE)
			type = POSITIVE_SAMPLER;

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
		IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order, type);

		Double weight;
		myRandom random;

		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		cerr << "Now sampling\n";
		//for(int samples=0;samples!=num_samples;samples++)
		double num_samples = 0;
		Double pe;
		while (1) {
			double sample_weight = 0.0;
			//double actual_weight=0.0;
			for (int i = 0; i < sampling_order.size(); i++) {
				int var = sampling_order[i];
				gm.variables[var]->value() = INVALID_VALUE;
			}
			for (int i = 0; i < sampling_order.size(); i++) {
				int var = sampling_order[i];
				Double curr_weight;
				int value;
				helper.getSample(var, value, curr_weight, random);
				gm.variables[var]->value() = value;

				sample_weight += log(curr_weight.value());
			}
			BE be(gm.variables, gm.functions, order);
			//cout<<log10(be.log_pe.toDouble().value())<<" "<<flush;
			long double actual_weight = be.log_pe.toLongdouble();
			weight += (exp(actual_weight - sample_weight));
			num_samples += (double) 1.0;
			pe = Double(weight / num_samples);
			pe *= gm.mult_factor;
			if (timer.timeout(time_bound))
				break;
		}
		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		cout << "M= " << gm.mult_factor << ", Z = " << log10(pe.value())
				<< " #Samples = " << num_samples << endl;
		return log10(pe.value());
	}

/*
	void computeBeliefs(GM& gm, JG& jg, int p_bound, vector<int>& order,
			vector<int>& sampling_order, clock_t& time_bound, ostream& out)
	{
		Timer timer;
		timer.start();
		marginals = vector<vector<Double> > (gm.variables.size());
		for (int i = 0; i < marginals.size(); i++) {
			marginals[i] = vector<Double> (gm.variables[i]->domain_size());
		}

		SAMPLER_TYPE type = ZERO_SAMPLER;
		if (gm.mode == POSITIVE)
			type = POSITIVE_SAMPLER;
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
		IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order, type);

		Double weight;
		myRandom random;

		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		while (1) {
			long double sample_weight = 0.0;
			for (int i = 0; i < sampling_order.size(); i++) {
				int var = sampling_order[i];
				Double curr_weight;
				int value;
				helper.getSample(var, value, curr_weight, random);
				gm.variables[var]->value() = value;
				sample_weight += log(curr_weight.value());
			}
			BESample be_sample(gm.variables, gm.functions, order, random);
			long double actual_weight = be_sample.log_pe.toLongdouble();
			weight = Double(exp(actual_weight - sample_weight));
			updateWeight(gm, weight);
			for (int i = 0; i < gm.variables.size(); i++) {
				gm.variables[i]->value() = INVALID_VALUE;
			}
			if (timer.timeout(time_bound))
				break;
		}
		gm.printMarginalsUAI10(marginals, out);
	}
*/
	void computeBeliefs_Improved(GM& gm, JG& jg, int p_bound, vector<int>& order, vector<set<int> >& clusters,
				vector<int>& sampling_order, clock_t& time_bound, ostream& out)
		{
			Timer timer;
			timer.start();
			marginals = vector<vector<Double> > (gm.variables.size());
			for (int i = 0; i < marginals.size(); i++) {
				marginals[i] = vector<Double> (gm.variables[i]->domain_size());
			}

			//SAMPLER_TYPE type = ZERO_SAMPLER;
			SAMPLER_TYPE type = POSITIVE_SAMPLER;
			if (gm.mode == POSITIVE)
				type = POSITIVE_SAMPLER;
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
			IJGPSamplingHelper helper(&gm, &jg, p_bound, helper_order, type);

			Double weight;
			myRandom random;

			for (int i = 0; i < gm.variables.size(); i++) {
				gm.variables[i]->value() = INVALID_VALUE;
			}
			JT jt(gm.variables, gm.functions, clusters, order);
			double num_samples=0;
			while (1) {
				long double sample_weight = 0.0;
				for (int i = 0; i < sampling_order.size(); i++) {
					int var = sampling_order[i];
					Double curr_weight;
					int value;
					helper.getSample(var, value, curr_weight, random);
					gm.variables[var]->value() = value;
					sample_weight += log(curr_weight.value());
				}
				BE be(gm.variables, gm.functions, order);
				long double actual_weight = be.log_pe.toLongdouble();


				jt.propagate();

				weight = Double(exp(actual_weight - sample_weight));
				assert(marginals.size()==jt.marginals.size());
				for(int i=0;i<marginals.size();i++){
					if (is_sampled[i]){
						marginals[i][gm.variables[i]->value()]+=weight;
					}
					else{
						for(int j=0;j<gm.variables[i]->domain_size();j++){
							marginals[i][j]+=jt.marginals[i].tableEntry(j)*weight;
						}
					}
				}
				for (int i = 0; i < gm.variables.size(); i++) {
					gm.variables[i]->value() = INVALID_VALUE;
				}
				if (timer.timeout(time_bound))
					break;
				num_samples+=1;
				//cout<<"Next iter\n";
			}
			cerr<<"Num Samples = "<<num_samples<<endl;
			gm.printMarginalsUAI10(marginals, out);
		}


};
}
#endif
