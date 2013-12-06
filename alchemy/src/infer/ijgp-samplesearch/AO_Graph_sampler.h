#ifndef SS_AO_Graph_SAMPLER_H_
#define SS_AO_Graph_SAMPLER_H_

#include "GM.h"
#include "JG.h"
#include "AOIJGPSamplingHelper.h"
#include "myRandom.h"
#include "AOGraph.h"
#include "SampleSearch.h"
#include "AO_IJGP_Sampler.h"
#include "AOGraph_BigInt.h"

namespace ss{

//struct AOSTATS
//{
//	Double or_num_samples;
//	Double and_or_num_samples;
//	Double or_pe;
//	Double and_or_pe;
//	Double or_time;
//	Double and_or_time;
//	AOSTATS(){}
//	void printStats(ostream& out)
//	{
//		out<<or_num_samples<<"\t"<<or_pe<<"\t"<<or_time<<"\t";
//		out<<and_or_num_samples<<"\t"<<and_or_pe<<"\t"<<and_or_time<<endl;
//	}
//};
class AO_Graph_Sampler
{
public:
	AO_Graph_Sampler():type(POSITIVE_SAMPLER){ }
	AOSTATS stats;
	SAMPLER_TYPE type;
	//void computeBeliefs(GM& gm, JG& jg, int p_bound, vector<int>& order, int time_limit, int num_samples,vector<vector<Double> >& beliefs)
	//{
	//	IJGPSamplingHelper helper(&gm,&jg,p_bound,order);
	//	vector<int> sampling_order[order.size()];
	//	int count=0;
	//	for(int i=order.size()-1;i>-1;i--)
	//	{
	//		sampling_order[count++]=order[i];
	//	}
	//	PseudoTree pseudo_tree(gm,sampling_order);
	//	AOTree aotree(pseudo_tree,gm,sampling_order);
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
	//			return;
	//		}
	//		// Initialize  
	//		for(int i=0;i<gm.variables.size();i++)
	//		{
	//			gm.variables[i]->value()=INVALID_VALUE;
	//		}
	//		double sample_weight=0.0;
	//		double actual_weight=0.0;
	//		vector<Double> q_value(order.size());
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
	//			q_value[var]=curr_weight;
	//		}
	//		aotree.addAssignment(q_value);
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
	Double computePE(GM& gm, JG& jg, int p_bound, vector<int>& order, PseudoTree& pseudo_tree, int time_limit,  int num_samples=INVALID_VALUE)
	{
		AO_IJGPSamplingHelper helper(&gm,&jg,p_bound,order,pseudo_tree,type);
		ofstream pstreefile("pstreefile");
		pseudo_tree.print(pstreefile);
		pstreefile.close();
		vector<int> sampling_order(order.size());
		int count=0;
		for(int i=order.size()-1;i>-1;i--)
		{
			sampling_order[count++]=order[i];
		}
		//AOTree aotree(pseudo_tree,gm,sampling_order);
		Graph g;
		jg.getGraph(gm,g);
		AOGraphBigInt aograph(gm,g,order);
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
			vector<Double> q_value(order.size());
			for(int i=0;i<gm.variables.size();i++)
			{
				int var=order[order.size()-1-i];

				Double curr_weight;
				int value;
				helper.getSample(var,value,curr_weight,random);
				gm.variables[var]->value()=value;
				sample_weight+=log(curr_weight.value());
				q_value[var]=curr_weight;
				

			}
			
			aograph.addAssignment(q_value);
			//cout<<endl;
			//cerr<<"Sample Weight = "<<sample_weight<<endl;
			for(int i=0;i<gm.functions.size();i++)
			{
				Double tmp=gm.functions[i]->getWeight();
				//cout<<tmp<<" ";
				//if(tmp.isZero())
				//{
				//	cerr<<"Actual Weight is zero\n";
				//	exit(1);
				//}
				actual_weight+=log(tmp.value());
			}
			//cout<<endl;
			//cerr<<"Actual weight = "<<actual_weight<<endl;
			weight+=(exp(actual_weight-sample_weight));
		}
		for(int i=0;i<gm.variables.size();i++)
		{
			gm.variables[i]->value()=INVALID_VALUE;
		}

		stats.or_pe=(weight/Double((double)num_samples));
		stats.or_pe*=gm.mult_factor;
		stats.or_num_samples=Double((double)num_samples);
		stats.or_time=Double((double)(curr_time-start_time));

		Double out_val(aograph.pe());

		curr_time=time(NULL);
		stats.and_or_pe=out_val;
		stats.and_or_pe*=gm.mult_factor;
		//stats.and_or_num_samples=aotree.root->num_samples;
		stats.and_or_time=Double((double)(curr_time-start_time));

		cout<<"OR samples = "<<num_samples<<endl;
		//cout<<"AND/OR samples = "<<aotree.root->num_samples<<endl;
		cout<<"OR pe = "<<log(stats.or_pe.value())<<endl;
		cout<<"AND/OR pe = "<<log(stats.and_or_pe.value())<<endl;

		return out_val;
	}

	// Compute P(e) by SampleSearch and AND/OR Sampling
	Double computePESS(GM& gm, JG& jg, int p_bound, vector<int>& order, PseudoTree& pseudo_tree, int time_limit,  int num_samples=INVALID_VALUE)
	{

		AO_IJGPSamplingHelper helper(&gm,&jg,p_bound,order,pseudo_tree,type);
		ofstream pstreefile("pstreefile");
		pseudo_tree.print(pstreefile);
		pstreefile.close();
		vector<int> sampling_order(order.size());
		int count=0;
		for(int i=order.size()-1;i>-1;i--)
		{
			sampling_order[count++]=order[i];
		}
		Graph g;
		jg.getGraph(gm,g);
		AOGraphBigInt aograph(gm,g,order);
		Double weight;
		myRandom random;
		time_t start_time,curr_time;
		start_time=time(NULL);
		SampleSearch ss(gm,jg,p_bound,order);
		for(int samples=0;samples!=num_samples;samples++)
		{
			//if(samples % 1000 ==0)
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
			vector<Double> q_value;
			ss.getSample(q_value);
			for(int i=0;i<gm.variables.size();i++)
			{
				//int var=order[order.size()-1-i];
				//Double curr_weight;
				//int value;
				//helper.getSample(var,value,curr_weight,random);
				//gm.variables[var]->value()=value;
				//sample_weight+=log(curr_weight.value());
				//q_value[var]=curr_weight;
				sample_weight+=log(q_value[i].value());
			}
			aograph.addAssignment(q_value);
			//cout<<endl;
			//cerr<<"Sample Weight = "<<sample_weight<<endl;
			for(int i=0;i<gm.functions.size();i++)
			{
				Double tmp=gm.functions[i]->getWeight();
				//cout<<tmp<<" ";
				//if(tmp.isZero())
				//{
				//	cerr<<"Actual Weight is zero\n";
				//	exit(1);
				//}
				actual_weight+=log(tmp.value());
			}
			//cout<<endl;
			//cerr<<"Actual weight = "<<actual_weight<<endl;
			weight+=Double(exp(actual_weight-sample_weight));
		}
		for(int i=0;i<gm.variables.size();i++)
		{
			gm.variables[i]->value()=INVALID_VALUE;
		}

		stats.or_pe=(weight/Double((double)num_samples));
		stats.or_pe*=gm.mult_factor;
		stats.or_num_samples=Double((double)num_samples);
		stats.or_time=Double((double)(curr_time-start_time));

		Double out_val(aograph.pe());

		curr_time=time(NULL);
		stats.and_or_pe=out_val;
		stats.and_or_pe*=gm.mult_factor;
		//stats.and_or_num_samples=aotree.root->num_samples;
		stats.and_or_time=Double((double)(curr_time-start_time));

		cout<<"OR samples = "<<num_samples<<endl;
		//cout<<"AND/OR samples = "<<aotree.root->num_samples<<endl;
		cout<<"OR pe = "<<log(stats.or_pe.value())<<endl;
		cout<<"AND/OR pe = "<<log(stats.and_or_pe.value())<<endl;

		return out_val;
	}
};
}
#endif
