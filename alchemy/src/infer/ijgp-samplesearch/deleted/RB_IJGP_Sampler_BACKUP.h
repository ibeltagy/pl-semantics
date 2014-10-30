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
	void updateWeight(GM& gm, vector<int>& sampled_variables,
			vector<int>& other_variables, Double& weight) {
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

		IJGPSamplingHelper helper(&gm, &jg, p_bound, order, type);
		Double weight;
		myRandom random;
		time_t start_time, curr_time;
		start_time = time(NULL);
		curr_time = time(NULL);
		set<int> sampled_variables;
		vector<int> other_variables;

		vector<bool> is_var_sampled(gm.variables.size());
		for (int i = 0; i < sampling_order.size(); i++){
			sampled_variables.insert(sampling_order[i]);
			is_var_sampled[sampling_order[i]] = true;
		}

		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}

		for (int i = 0; i < order.size(); i++) {
			if (sampled_variables.find(i) == sampled_variables.end())
				other_variables.push_back(i);
		}
		cerr << "Now sampling\n";
		//for(int samples=0;samples!=num_samples;samples++)
		double num_samples = 0;
		Double pe;
		while (1)
		{
			//if(samples % 1000 ==0)
			//	cerr<<"Sample "<<samples<<" taken\n";
			//curr_time=time(NULL);
			//if(num_samples==INVALID_VALUE && (curr_time-start_time) >= time_limit)
			//{
			//	cerr<<"Time = "<<curr_time-start_time<<endl;
			//	for(int i=0;i<gm.variables.size();i++)
			//	{
			//		gm.variables[i]->value()=INVALID_VALUE;
			//	}
			//	return weight/Double((double)samples);
			//}
			// Initialize
			for (int i = 0; i < gm.variables.size(); i++) {
				gm.variables[i]->value() = INVALID_VALUE;
			}
			double sample_weight = 0.0;
			//double actual_weight=0.0;
			for (int i = 0; i < gm.variables.size(); i++) {
				int var = order[order.size() - 1 - i];
				//int var=sampling_order[i];
				Double curr_weight;
				int value;
				helper.getSample(var, value, curr_weight, random);
				gm.variables[var]->value() = value;
				if (is_var_sampled[var])
					sample_weight += log(curr_weight.value());
			}

			for (int i = 0; i < other_variables.size(); i++)
				gm.variables[other_variables[i]]->value() = INVALID_VALUE;

			BE be(gm.variables, gm.functions, order);
			cout<<log10(be.log_pe.toDouble().value())<<" "<<flush;
			double actual_weight = log(be.log_pe.toDouble().value());
			weight += (exp(actual_weight - sample_weight));
			num_samples += (double) 1.0;
			pe = Double(weight/num_samples);
			pe *= gm.mult_factor;
			if (timer.timeout(time_bound))
				break;
		}
		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		cout<<"Z = "<<log10(pe.value())<<" #Samples = "<<num_samples<<endl;
		return log10(pe.value());
	}

	Double computeBeliefs(GM& gm, JG& jg, int p_bound,
			vector<set<int> >& clusters, vector<int>& order,
			vector<int>& sampling_order, int time_limit, int num_samples =
					INVALID_VALUE) {
		marginals = vector<vector<Double> > (gm.variables.size());
		for (int i = 0; i < marginals.size(); i++) {
			marginals[i] = vector<Double> (gm.variables[i]->domain_size());
		}
		cout << "t " << getDateAndTime() << endl;
		SAMPLER_TYPE type = ZERO_SAMPLER;
		if (gm.mode == POSITIVE)
			type = ZERO_SAMPLER;
		IJGPSamplingHelper helper(&gm, &jg, p_bound, order, type);
		Double weight;
		myRandom random;
		time_t start_time, curr_time;
		start_time = time(NULL);
		curr_time = time(NULL);
		set<int> sampled_variables;
		vector<int> other_variables;
		for (int i = 0; i < sampling_order.size(); i++)
			sampled_variables.insert(sampling_order[i]);

		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}

		for (int i = 0; i < order.size(); i++) {
			if (sampled_variables.find(i) == sampled_variables.end())
				other_variables.push_back(i);
		}
		cerr << "Now sampling\n";
		JT jt(gm.variables, gm.functions, clusters, order);
		for (int samples = 0; samples != num_samples; samples++) {
			if (samples % 1000 == 0)
				cerr << "Sample " << samples << " taken\n";
			//curr_time=time(NULL);
			//if(num_samples==INVALID_VALUE && (curr_time-start_time) >= time_limit)
			//{
			//	cerr<<"Time = "<<curr_time-start_time<<endl;
			//	for(int i=0;i<gm.variables.size();i++)
			//	{
			//		gm.variables[i]->value()=INVALID_VALUE;
			//	}
			//	return weight/Double((double)samples);
			//}
			// Initialize
			for (int i = 0; i < gm.variables.size(); i++) {
				gm.variables[i]->value() = INVALID_VALUE;
			}
			long double sample_weight = 0.0;
			//double actual_weight=0.0;
			for (int i = 0; i < gm.variables.size(); i++)
			//for(int i=0;i<sampling_order.size();i++)
			{
				int var = order[order.size() - 1 - i];
				Double curr_weight;
				int value;
				helper.getSample(var, value, curr_weight, random);
				gm.variables[var]->value() = value;
				//cout<<i<<endl;
				//cout<<curr_weight<<" ";
				//if(curr_weight.isZero())
				//{
				//	cerr<<"Weight is zero\n";
				//	exit(1);
				//}
				if (sampled_variables.find(var) != sampled_variables.end())
					sample_weight += log(curr_weight.value());
			}
			for (int i = 0; i < other_variables.size(); i++)
				gm.variables[other_variables[i]]->value() = INVALID_VALUE;
			BESample be_sample(gm.variables, gm.functions, order, random);
			//jt.propagate();
			//BE be(gm.variables,gm.functions,order);
			long double actual_weight = be_sample.log_pe.toLongdouble();
			//log(be.log_pe.toDouble().value());
			//cout<<endl;
			//cerr<<"Sample Weight = "<<sample_weight<<endl;
			//for(int i=0;i<gm.functions.size();i++)
			//{
			//	Double tmp=gm.functions[i]->getWeight();
			//	//if(tmp.isZero())
			//	//{
			//	//	cerr<<"Actual Weight is zero\n";
			//	//	exit(1);
			//	//}
			//	actual_weight+=log(tmp.value());
			//}
			//cerr<<"Actual weight = "<<actual_weight<<endl;
			weight = Double(exp(actual_weight - sample_weight));
			updateWeight(gm, sampling_order, other_variables, weight);
			for (int i = 0; i < gm.variables.size(); i++) {
				gm.variables[i]->value() = INVALID_VALUE;
			}
			time_t some_time = time(NULL);
			if (some_time > (curr_time + 60)) {
				curr_time = time(NULL);
				cout << "t " << getDateAndTime() << endl;
				gm.printMarginalsUAI08(marginals);
			}

			//Double pe=weight/Double((double)samples+1);
			//pe*=gm.mult_factor;
			//cout.setf(ios::fixed,ios::floatfield);
			//cout<<" z "<<log10(pe.value())<<"\n";
		}
		for (int i = 0; i < gm.variables.size(); i++) {
			gm.variables[i]->value() = INVALID_VALUE;
		}
		return weight / Double((double) num_samples);
	}

};
}
#endif
