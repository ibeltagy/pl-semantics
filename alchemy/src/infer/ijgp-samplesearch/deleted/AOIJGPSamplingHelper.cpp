#include "AOIJGPSamplingHelper.h"

namespace ss{

AO_POS::AO_POS(GM* gm, vector<int>& order,JG* jg, vector<vector<Variable*> >& ancestors)
{
	//vector<Variable*> all_variables;
	//cout<<"\t "<<gm->variables.size()<<endl;
	sampling_functions=vector<SF>(gm->variables.size()); 
	for(int i=0;i<gm->variables.size();i++)
	{
		int var=order[order.size()-1-i];
		cerr<<var<<" ";
		vector<Variable*>& all_variables=ancestors[var];
		//all_variables.push_back(gm->variables[var]);
		//sort(all_variables.begin(),all_variables.end(),less_than_comparator_variable);
		// Find the largest cluster in the join graph which has a non-zero intersection with all_variables
		int max_cluster_id=INVALID_VALUE;
		int max=INVALID_VALUE;
		for(int j=0;j<jg->nodes.size();j++)
		{
			bool found=false;
			for(int k=0;k<jg->nodes[j]->variables().size();k++)
			{
				if(jg->nodes[j]->variables()[k]->id()==var)
				{
					found=true;
					max_cluster_id=j;
					break;
				}
			}
			if(found)
			{
				//break;
				//cout<<"Variable found "<<var<<"\n";
				vector<Variable*> temp;
				do_set_intersection(all_variables,jg->nodes[j]->variables(),temp,less_than_comparator_variable);
				if((int)temp.size() > max)
				{
					max=(int)temp.size();
					max_cluster_id=j;
				}
			}
		}
		//max_cluster_id=INVALID_VALUE;
		if(max_cluster_id == INVALID_VALUE)
		{
			//jg->print(cout);
			//cout<<"invalid value for var = "<<var<<endl;
			vector<Variable*> cond_vars;
			CPT cpt;
			cpt.variables().push_back(gm->variables[var]);
			cpt.cond_variables()=vector<Variable*>();
			cpt.setMargVariable(gm->variables[var]);
			cpt.table()=vector<Double> (cpt.marg_variable()->domain_size());
			for(int j=0;j<cpt.table().size();j++)
			{
				cpt.table()[j]=Double((double)1.0/(double)cpt.marg_variable()->domain_size());
			}
			//cpt.epsilonCorrection();
			sampling_functions[var]=SF(cpt);
			continue;
		}
		assert(max_cluster_id > INVALID_VALUE);
		//Form the function
		vector<Variable*> temp;
		do_set_intersection(all_variables,jg->nodes[max_cluster_id]->variables(),temp,less_than_comparator_variable);
		vector<Variable*> marg_vars;
		marg_vars.push_back(gm->variables[var]);
		do_set_difference(temp,marg_vars,temp,less_than_comparator_variable);
		
		CPT cpt;
		jg->nodes[max_cluster_id]->getCF(temp,gm->variables[var],cpt);
		sampling_functions[var]=SF(cpt);
	}
	cerr<<endl;
}

void AO_POS::getSample(const int& variable, int& value, Double& weight, myRandom& random)
{
	sampling_functions[variable].getSample(value,weight,random);	
}
Function& AO_POS::getFunction(int variable) 
{
	return sampling_functions[variable];
}

AO_COS::AO_COS(GM* gm, vector<int>& order, JG* jg,vector<vector<Variable*> >& ancestors)
{
	csp=new GM();
	gm->convertToCN(*csp);
	cp_algo=new DRC(*csp,jg->i_bound(),order);
	//vector<Variable*> all_variables;
	//cout<<"\t "<<gm->variables.size()<<endl;
	sampling_functions=vector<CPT>(gm->variables.size());
	for(int i=0;i<gm->variables.size();i++)
	{
		int var=order[order.size()-i-1];
		vector<Variable*>& all_variables=ancestors[var];
		//all_variables.push_back(gm->variables[var]);
		//sort(all_variables.begin(),all_variables.end(),less_than_comparator_variable);
		// Find the largest cluster in the join graph which has a non-zero intersection with all_variables
		int max_cluster_id=INVALID_VALUE;
		int max=INVALID_VALUE;
		for(int j=0;j<jg->nodes.size();j++)
		{
			bool found=false;
			for(int k=0;k<jg->nodes[j]->variables().size();k++)
			{
				if(jg->nodes[j]->variables()[k]->id()==var)
				{
					found=true;
					//max_cluster_id=j;
					
					break;
				}
			}
			if(found)
			{
				//break;
				//cout<<"Variable found "<<var<<"\n";
				vector<Variable*> temp;
				do_set_intersection(all_variables,jg->nodes[j]->variables(),temp,less_than_comparator_variable);
				if((int)temp.size() > max)
				{
					max=(int)temp.size();
					max_cluster_id=j;
				}
			}
		}
		
		//max_cluster_id=INVALID_VALUE;
		if(max_cluster_id == INVALID_VALUE)
		{
			//jg->print(cout);
			cout<<"invalid value for var = "<<var<<endl;
			vector<Variable*> cond_vars;
			CPT& cpt=sampling_functions[var];
			cpt.variables().push_back(gm->variables[var]);
			cpt.cond_variables()=vector<Variable*>();
			cpt.setMargVariable(gm->variables[var]);
			cpt.table()=vector<Double> (cpt.marg_variable()->domain_size());
			for(int j=0;j<cpt.table().size();j++)
			{
				cpt.table()[j]=Double((double)1.0/(double)cpt.marg_variable()->domain_size());
			}
			//cpt.epsilonCorrection();
	
			continue;
		}
		//cout<<i<<endl;
		assert(max_cluster_id > INVALID_VALUE);
		//Form the function
		vector<Variable*> temp;
		do_set_intersection(all_variables,jg->nodes[max_cluster_id]->variables(),temp,less_than_comparator_variable);
		vector<Variable*> marg_vars;
		marg_vars.push_back(gm->variables[var]);
		do_set_difference(temp,marg_vars,temp,less_than_comparator_variable);
		
		CPT& cpt=sampling_functions[var];
		
		jg->nodes[max_cluster_id]->getCF(temp,gm->variables[var],cpt);
		//Double epsilon(0.1);
		//cpt.epsilonCorrection(epsilon);
		
		
	
	}
}
void AO_COS::getSample(const int& variable, int& value, Double& weight,myRandom& random)
{
	Double epsilon(0.01);
	vector<Double> dist(csp->variables[variable]->domain_size());
	Double norm_const;
	for(int i=0;i<dist.size();i++)
	{
		if(cp_algo->isConsistent(variable,i))
		{
			csp->variables[variable]->addr_value()=i;
			int entry=Variable::getAddress(sampling_functions[variable].variables());
			
			dist[i]=sampling_functions[variable].table()[entry];
			norm_const+=dist[i];
			//dist[i]=Double(1.0);
		}
		
		
	}
	//cout<<"Var = "<<variable<<": ";
	//Double norm_const1;
	//for(int i=0;i<dist.size();i++)
	//{
	//	if(dist[i].isZero())
	//		continue;
	//	if((dist[i]/norm_const) < epsilon);
	//	{
	//		dist[i]=epsilon;
	//	}
	//	norm_const1+=dist[i];
	//	//cout<<dist[i]<<" ";
	//}
	for(int i=0;i<dist.size();i++)
		dist[i]/=norm_const;
	/*for(int i=0;i<dist.size();i++)
	{
		dist[i]/=norm_const1;
		
	}
	*/
	//cout<<endl;
	double sampled_value=random.getDouble();
	double cdf=0.0;
	for(int i=0;i<dist.size();i++)
	{
		cdf+=dist[i].value();
		if(cdf >= sampled_value)
		{
			value=i;
			weight=dist[i];
			return;
		}
	}
	//cerr<<"Should not reach here---Rejection problem\n";
	value=0;
	weight=Double();
}
Function& AO_COS::getFunction(int variable)
{
	return sampling_functions[variable];
}
void AO_COSP::buildSF(int curr_variable)
{
	cout<<"BuildSF...."<<curr_variable<<" "<<flush;
	int pos_curr_variable=mapped_order[curr_variable];
	if(!run_ijgp[pos_curr_variable])
		return;
	jg->propagate();
	vector<Variable*> all_variables;
	for(int i=pos_curr_variable;i>-1;i--)
	{
		if(run_ijgp[i] && i!=pos_curr_variable)
			return;
		
		int var=order[i];
		cout<<"Var = "<<var<<endl;
		all_variables.push_back(gm->variables[var]);
		sort(all_variables.begin(),all_variables.end(),less_than_comparator_variable);
		int max=INVALID_VALUE;
		int max_cluster_id=INVALID_VALUE;
		for(int j=0;j<jg->nodes.size();j++)
		{
			bool found=false;
			for(int k=0;k<jg->nodes[j]->variables().size();k++)
			{
				if(jg->nodes[j]->variables()[k]->id()==var)
				{
					found=true;
					break;
				}
			}
			if(found)
			{
				vector<Variable*> temp;
				do_set_intersection(all_variables,jg->nodes[j]->variables(),temp,less_than_comparator_variable);
				if((int)temp.size() > max)
				{
					max=(int)temp.size();
					max_cluster_id=j;
				}
			}
		}
		if(max_cluster_id == INVALID_VALUE)
		{
			//jg->print(cout);
			cout<<"invalid value for var = "<<var<<endl;
			vector<Variable*> cond_vars;
			CPT& cpt=sampling_functions[var];
			cpt.variables().push_back(gm->variables[var]);
			cpt.cond_variables()=vector<Variable*>();
			cpt.setMargVariable(gm->variables[var]);
			cpt.table()=vector<Double> (cpt.marg_variable()->domain_size());
			for(int j=0;j<cpt.table().size();j++)
			{
				cpt.table()[j]=Double((double)1.0/(double)cpt.marg_variable()->domain_size());
			}
		
	
			continue;
		}
		
		assert(max_cluster_id > INVALID_VALUE);
		//Form the function
		vector<Variable*> temp;
		do_set_intersection(all_variables,jg->nodes[max_cluster_id]->variables(),temp,less_than_comparator_variable);
		vector<Variable*> marg_vars;
		marg_vars.push_back(gm->variables[var]);
		do_set_difference(temp,marg_vars,temp,less_than_comparator_variable);
		CPT& cpt=sampling_functions[var];
		jg->nodes[max_cluster_id]->getCF(temp,gm->variables[var],cpt);
	}
}
AO_COSP::AO_COSP(GM* gm_, vector<int>& order_, JG* jg_, vector<vector<Variable*> >& ancestors, int p_):gm(gm_),order(order_),jg(jg_),p(p_)
{
	myRandom random;
	cout<<" p is --- "<<p<<endl;
	//Create the constraint network and run DRC on it
	csp=new GM();
	gm->convertToCN(*csp);
	cp_algo=new DRC(*csp,jg->i_bound(),order);
	// Store at what point in ordering do you want to propagate
	run_ijgp=vector<bool> (order.size());
	mapped_order=vector<int> (order.size());
	for(int i=0;i<order.size();i++)
	{
		run_ijgp[i]=(random.getInt(100)<p)?(true):(false);
		if(i==((int)order.size()-1)) run_ijgp[i]=true;
		mapped_order[order[i]]=i;
	}

	sampling_functions=vector<CPT>(gm->variables.size());
	if(!order.empty())
	{
		buildSF(order[order.size()-1]);
	}
}

void AO_COSP::getSample(const int& variable, int& value, Double& weight,myRandom& random)
{
	buildSF(variable);
	
	vector<Double> dist(csp->variables[variable]->domain_size());
	Double norm_const;
	for(int i=0;i<dist.size();i++)
	{
		if(cp_algo->isConsistent(variable,i))
		{
			csp->variables[variable]->addr_value()=i;
			int entry=Variable::getAddress(sampling_functions[variable].variables());
			
			dist[i]=sampling_functions[variable].table()[entry];
			norm_const+=dist[i];
			//dist[i]=Double(1.0);
		}
	}
	for(int i=0;i<dist.size();i++)
	{
		dist[i]/=norm_const;
	}
	
	double sampled_value=random.getDouble();
	double cdf=0.0;
	for(int i=0;i<dist.size();i++)
	{
		cdf+=dist[i].value();
		if(cdf >= sampled_value)
		{
			value=i;
			weight=dist[i];
			return;
		}
	}
	
	value=0;
	weight=Double();
}
Function& AO_COSP::getFunction(int variable)
{
	return sampling_functions[variable];
}

AO_IJGPSamplingHelper::AO_IJGPSamplingHelper(GM* gm_, JG* jg_, int p_, vector<int>& order, PseudoTree& pseudo_tree, SAMPLER_TYPE type)
:gm(gm_),
jg(jg_),
p(p_)
{
	pseudo_tree.generateAncestors(*gm,ancestors);
	/*cout<<"Ancestors\n";
	for(int i=order.size()-1;i>-1;i--)
	{
		int var=order[i];
		cout<<"Anc of var "<<var<<" are: ";
		for(int j=0;j<ancestors[var].size();j++)
			cout<<ancestors[var][j]->id()<<" ";
		cout<<endl;
	}*/
	//cerr<<"Propagating\n";
	//jg->propagate();
	//cerr<<"Propagation done\n";
	if(p<0)
	{
		p=0;
		//return;
	}
	else if(p > 100)
	{
		p=100;
	}
	cout<<"p is "<<p<<endl;
	//if(sampler!=NULL)
	//	delete(sampler);
	switch(type)
	{
	case POSITIVE_SAMPLER:
		if(p==0)
		{
			sampler=new AO_POS(gm_,order,jg_,ancestors);
		}
		else
		{
			sampler=new AO_COSP(gm_,order,jg_,ancestors,p);
		}
		break;
	case ZERO_SAMPLER:
		if(p==0)
		{
			sampler=new AO_COS(gm_,order,jg_,ancestors);
		}
		else
		{
			sampler=new AO_COSP(gm_,order,jg_,ancestors,p);
		}
		break;
	default:
		if(p==0)
		{
			sampler=new AO_COS(gm_,order,jg_,ancestors);
		}
		else
		{
			sampler=new AO_COSP(gm_,order,jg_,ancestors,p);
		}
		break;
	}
}
}
