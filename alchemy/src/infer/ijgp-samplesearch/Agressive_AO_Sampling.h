#ifndef AGRESSIVE_AO_SAMPLING_H_
#define AGRESSIVE_AO_SAMPLING_H_

#include "GM.h"
#include "JG.h"
#include "AOIJGPSamplingHelper.h"
#include "myRandom.h"
#include "AOTree.h"
#include "SampleSearch.h"
#include "AO_IJGP_Sampler.h"
#include <stack>

struct SepTreeNode
{
	vector<int> variables;
	vector<SepTreeNode*> children;
	int level;
	SepTreeNode():level(-1){}
};
struct SepTree
{
	vector<vector<SepTreeNode*> > nodes_at_level;
	int height() { return nodes_at_level.size();}
	void print(ostream& out)
	{
		stack<SepTreeNode*> s;
		s.push(nodes_at_level[0][0]);
		while(!s.empty())
		{
			SepTreeNode* sepnode=s.top();
			s.pop();
			out<<"Level "<<sepnode->level<<": ";
			for(int i=0;i<sepnode->variables.size();i++)
			{
				out<<sepnode->variables[i]<<" ";
			}
			out<<endl;
			for(int i=0;i<sepnode->children.size();i++)
				s.push(sepnode->children[i]);
		}
		for(int j=0;j<nodes_at_level.size();j++)
		{
			for(int k=0;k<nodes_at_level[j].size();k++)
			{
				out<<"Level "<<j<<": ";
				for(int r=0;r<nodes_at_level[j][k]->variables.size();r++)
				{
					out<<nodes_at_level[j][k]->variables[r]<<" ";
				}
				out<<endl;
			}
		}
		/*for(int j=0;j<nodes_at_level.size();j++)
		{
			for(int k=0;k<nodes_at_level[j].size();k++)
			{
				for(int r=0;r<nodes_at_level[j][k]->variables.size();r++)
				{
					all_vars.push_back(nodes_at_level[j][k]->variables[r]);
				}
			}
		}*/

	}
	SepTree(PseudoTree* ps)
	{
		
		SepTreeNode* root=new SepTreeNode();
		stack<PseudoTreeNode*> s;
		s.push(ps->root);

		stack<SepTreeNode*> sep_stack;
		sep_stack.push(root);
		root->level=1;
		nodes_at_level.resize(1);
		nodes_at_level[0].push_back(root);
		while(!s.empty())
		{
			PseudoTreeNode* psnode=s.top();
			s.pop();
			SepTreeNode* sepnode=sep_stack.top();
			sepnode->variables.push_back(psnode->variable->id());
			if((int)psnode->children.size() > 1)
			{
				sep_stack.pop();
				sepnode->children=vector<SepTreeNode*> (psnode->children.size());
				for(int i=0;i<sepnode->children.size();i++)
				{
					sepnode->children[i]=new SepTreeNode();
					sepnode->children[i]->level=sepnode->level+1;
					//Put the child in the appropriate level
					nodes_at_level.resize(sepnode->level+1);
					nodes_at_level[sepnode->level].push_back(sepnode->children[i]);
					sep_stack.push(sepnode->children[i]);
				}
			}
			for(int i=0;i<psnode->children.size();i++)
			{
				s.push(psnode->children[i]);
			}
			if(psnode->children.empty())
				sep_stack.pop();
		}
		print(cout);
	}
};
struct AGGRRESSIVE_AO_SAMPLING
{
	AOSTATS stats;
	AGGRRESSIVE_AO_SAMPLING(){}
	Double computePE(GM& gm, JG& jg, int p_bound, vector<int>& order, PseudoTree& pseudo_tree, int j_bound, int time_limit,  int num_samples=INVALID_VALUE)
	{
		AO_IJGPSamplingHelper helper(&gm,&jg,p_bound,order,pseudo_tree);
		ofstream pstreefile("pstreefile");
		pseudo_tree.print(pstreefile);
		pstreefile.close();
		vector<int> sampling_order(order.size());
		int count=0;
		for(int i=order.size()-1;i>-1;i--)
		{
			sampling_order[count++]=order[i];
		}
		AOTree aotree(pseudo_tree,gm,sampling_order);
		Double weight;
		myRandom random;
		time_t start_time,curr_time;
		start_time=time(NULL);
		
		SepTree sep_tree(&pseudo_tree);
		//right now we don't have a time based system and therefore we just control it based on number of samples
		assert(num_samples!=INVALID_VALUE);
		assert(j_bound < num_samples);

		vector<vector<int> > all_samples(num_samples);
		vector<vector<Double> > all_weights(num_samples);
		for(int i=0;i<all_samples.size();i++)
		{
			all_samples[i]=vector<int> (gm.variables.size());
			all_weights[i]=vector<Double> (gm.variables.size());
		}
		int h=sep_tree.height();

		// Number of samples in each cluster n;
		int n=(int)exp(log((double)num_samples)/h);

		n=(n< j_bound)?(j_bound):(n);
		
		
		
		//bool done=false;
		int a=0; // level
		int b=n; // num-samples
		
		
		count=0;
		while(1)
		{
			//Find all vars at this level
			vector<int> all_vars;
			for(int j=0;j<=a;j++)
			{
				for(int k=0;k<sep_tree.nodes_at_level[j].size();k++)
				{
					for(int r=0;r<sep_tree.nodes_at_level[j][k]->variables.size();r++)
					{
						all_vars.push_back(sep_tree.nodes_at_level[j][k]->variables[r]);
					}
				}
			}
			count++;
			cout<<"Count = "<<count<<endl;
			assert(b<=num_samples);
			// At each level you generate b samples
			for(int i=0;i<b;i++)
			{
				// First set all the variables upto level a-1
				for(int j=0;j<a;j++)
				{
					for(int k=0;k<sep_tree.nodes_at_level[j].size();k++)
					{
						for(int r=0;r<sep_tree.nodes_at_level[j][k]->variables.size();r++)
						{
							gm.variables[sep_tree.nodes_at_level[j][k]->variables[r]]->value()=all_samples[sep_tree.nodes_at_level[j][k]->variables[r]][b];
						}
					}
				}
				// Now sample variables at level a
				for(int j=0;j<sep_tree.nodes_at_level[a].size();j++)
				{
					for(int r=0;r<sep_tree.nodes_at_level[a][j]->variables.size();r++)
					{
						int var=sep_tree.nodes_at_level[a][j]->variables[r];
						Double curr_weight;
						int value;
						helper.getSample(var,value,curr_weight,random);
						gm.variables[var]->value()=value;
						all_samples[i][var]=value;
						all_weights[i][var]=curr_weight;

					}
				}
			}
			
			// Incremenet level
			a++;
			// Exit conditions
			if(a==h)
				break;
			if(b*n >= num_samples)
			{
				break;
			}
			
			// At the next level, we need to generate b*n samples
			// Therefore make n copies of the b samples
			for(int i=0;i<b;i++)
			{
				for(int j=1;j<n;j++)
				{
					for(int k=0;k<all_vars.size();k++)
					{
						all_samples[i+b*j][all_vars[k]]=all_samples[i][all_vars[k]];
						all_weights[i+b*j][all_vars[k]]=all_weights[i][all_vars[k]];
					}
				}
			}
			// Increment b
			b*=n;
		}
		cout<<"Init sampling done\n";
		// Generate b samples in remaining clusters if any
		
		for(int i=0;i<b&&a<h;i++)
		{
			// First set all the variables upto level a-1
			for(int j=0;j<a;j++)
			{
				for(int k=0;k<sep_tree.nodes_at_level[j].size();k++)
				{
					for(int r=0;r<sep_tree.nodes_at_level[j][k]->variables.size();r++)
					{
						int var=sep_tree.nodes_at_level[j][k]->variables[r];
						assert(all_samples[i][var] < gm.variables[var]->domain_size());
						gm.variables[var]->value()=all_samples[i][var];
					}
				}
			}
			// Then sample the remanining variables from level a onwards
			for(int j=a;j<h;j++)
			{
				for(int k=0;k<sep_tree.nodes_at_level[j].size();k++)
				{
					for(int r=0;r<sep_tree.nodes_at_level[j][k]->variables.size();r++)
					{
						int var=sep_tree.nodes_at_level[j][k]->variables[r];
						Double curr_weight;
						int value;
						helper.getSample(var,value,curr_weight,random);
						gm.variables[var]->value()=value;
						all_samples[i][var]=value;
						all_weights[i][var]=curr_weight;

					}
				}
			}
		}
		cout<<"All samples done\n";
		// Now put all samples in the aotree
		for(int i=0;i<b;i++)
		{
			double sample_weight=0.0;
			double actual_weight=0.0;
		     
			for(int j=0;j<all_samples[i].size();j++)
			{
				gm.variables[j]->value()=all_samples[i][j];
			//	cout<<all_samples[i][j]<<" ";
				sample_weight+=log(all_weights[i][j].value());
			}
			//cout<<endl;
			aotree.addAssignment(all_weights[i]);
		
			for(int r=0;r<gm.functions.size();r++)
			{
				Double tmp=gm.functions[r]->getWeight();
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
			// Free memory
			all_samples[i].clear();
			all_weights[i].clear();
		}
		all_samples.clear();
		all_weights.clear();
		
		// Generate the remaining samples
		for(int x=b;x<num_samples;x++)
		{
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
			for(int r=0;r<gm.functions.size();r++)
			{
				Double tmp=gm.functions[r]->getWeight();
				//cout<<tmp<<" ";
				//if(tmp.isZero())
				//{
				//	cerr<<"Actual Weight is zero\n";
				//	exit(1);
				//}
				actual_weight+=log(tmp.value());
			}
			weight+=(exp(actual_weight-sample_weight));
			aotree.addAssignment(q_value);
		}
		curr_time=time(NULL);
		stats.or_pe=(weight/Double((double)num_samples));
		stats.or_pe*=gm.mult_factor;
		stats.or_num_samples=Double((double)num_samples);
		stats.or_time=Double((double)(curr_time-start_time));

		Double out_val(aotree.computeWeight());

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
#endif