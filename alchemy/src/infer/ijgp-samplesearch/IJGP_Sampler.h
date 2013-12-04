#ifndef IJGP_SAMPLER_H_
#define IJGP_SAMPLER_H_

#include "GM.h"
#include "JG.h"
#include "IJGPSamplingHelper.h"
#include "myRandom.h"
class IJGP_Sampler
{
public:
	IJGP_Sampler(){ }
	void computeBeliefs(GM& gm, JG& jg, int p_bound, vector<int>& order, int time_limit,  int num_samples,vector<vector<Double> >& beliefs)
	{
		IJGPSamplingHelper helper(&gm,&jg,p_bound,order);
		Double weight;
		myRandom random;
		time_t start_time,curr_time;
		start_time=time(NULL);
		beliefs=vector<vector<Double> > (gm.variables.size());
		for(int i=0;i<gm.variables.size();i++)
		{
			beliefs[i]=vector<Double> (gm.variables[i]->domain_size());
		}
	
		for(int samples=0;samples<num_samples;samples++)
		{
			if(samples % 1000 ==0)
				cerr<<"Sample "<<samples<<" taken\n";
			curr_time=time(NULL);
			if(num_samples==INVALID_VALUE && (curr_time-start_time) >= time_limit)
			{
			
				return;
			}
			// Initialize  ;'v;vp-pkvv;
			for(int i=0;i<gm.variables.size();i++)
			{
				gm.variables[i]->value()=INVALID_VALUE;
			}
			double sample_weight=0.0;
			double actual_weight=0.0;
			for(int i=0;i<gm.variables.size();i++)
			{
				int var=order[order.size()-1-i];
				Double curr_weight;
				int value;
				helper.getSample(var,value,curr_weight,random);
				gm.variables[var]->value()=value;
				//cout<<i<<endl;
				//cout<<curr_weight<<" ";
				//if(curr_weight.isZero())
				//{
				//	cerr<<"Weight is zero\n";
				//	exit(1);
				//}
				
				sample_weight+=log(curr_weight.value());
			}
			//cout<<endl;
			//cerr<<"Sample Weight = "<<sample_weight<<endl;
			for(int i=0;i<gm.functions.size();i++)
			{
				Double tmp=gm.functions[i]->getWeight();
				//if(tmp.isZero())
				//{
	
				//	cerr<<"Actual Weight is zero\n";
				//	exit(1);
				//}
				actual_weight+=log(tmp.value());
			}
			Double curr_weight(exp(actual_weight-sample_weight));
			for(int i=0;i<gm.variables.size();i++)
			{
				assert(gm.variables[i]->value()!=INVALID_VALUE);
				beliefs[i][gm.variables[i]->value()]+=curr_weight;	
			}	
			//cerr<<"Actual weight = "<<actual_weight<<endl;
			weight+=(exp(actual_weight-sample_weight));
		}
		for(int i=0;i<gm.variables.size();i++)
		{
			gm.variables[i]->value()=INVALID_VALUE;
		}
		for(int i=0;i<gm.variables.size();i++)
		{
			gm.variables[i]->value()=INVALID_VALUE;
			for(int j=0;j<beliefs[i].size();j++)
				beliefs[i][j]/=weight;
		}
		return;
	}
	Double computePE(GM& gm, JG& jg, int p_bound, vector<int>& order, int time_limit,  int num_samples=INVALID_VALUE)
	{
		IJGPSamplingHelper helper(&gm,&jg,p_bound,order);
		Double weight;
		myRandom random;
		time_t start_time,curr_time;
		start_time=time(NULL);
		for(int samples=0;samples!=num_samples;samples++)
		{
			if(samples % 1000 ==0)
				cerr<<"Sample "<<samples<<" taken\n";
			curr_time=time(NULL);
			if(num_samples==INVALID_VALUE && (curr_time-start_time) >= time_limit)
			{
				cerr<<"Time = "<<curr_time-start_time<<endl;
				for(int i=0;i<gm.variables.size();i++)
				{
					gm.variables[i]->value()=INVALID_VALUE;
				}
				return weight/Double((double)samples);
			}
			// Initialize
			for(int i=0;i<gm.variables.size();i++)
			{
				gm.variables[i]->value()=INVALID_VALUE;
			}
			double sample_weight=0.0;
			double actual_weight=0.0;
			for(int i=0;i<gm.variables.size();i++)
			{
				int var=order[order.size()-1-i];
				Double curr_weight;
				int value;
				helper.getSample(var,value,curr_weight,random);
				gm.variables[var]->value()=value;
				//cout<<i<<endl;
				//cout<<curr_weight<<" ";
				//if(curr_weight.isZero())
				//{
				//	cerr<<"Weight is zero\n";
				//	exit(1);
				//}
				sample_weight+=log(curr_weight.value());
			}
			//cout<<endl;
			//cerr<<"Sample Weight = "<<sample_weight<<endl;
			for(int i=0;i<gm.functions.size();i++)
			{
				Double tmp=gm.functions[i]->getWeight();
				//if(tmp.isZero())
				//{
				//	cerr<<"Actual Weight is zero\n";
				//	exit(1);
				//}
				actual_weight+=log(tmp.value());
			}
			//cerr<<"Actual weight = "<<actual_weight<<endl;
			weight+=(exp(actual_weight-sample_weight));
		}
		for(int i=0;i<gm.variables.size();i++)
		{
			gm.variables[i]->value()=INVALID_VALUE;
		}
		return weight/Double((double)num_samples);
	}
};
#endif