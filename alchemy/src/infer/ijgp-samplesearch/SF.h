#ifndef SF_H_
#define SF_H_

#include "CPT.h"
#include "myRandom.h"
class SF: public CPT
{
protected:
	vector<vector<Double> > sampling_table;
public:
	SF():CPT(){}
	SF(CPT& cpt);
	void getSample(int& value, Double& weight,myRandom& random);
	void print(ostream& out=cout)
	{
		out<<"Variables: ";
		for(int i=0;i<variables_.size();i++)
			out<<variables_[i]->id()<<" ";
		out<<endl;
		for(int i=0;i<sampling_table.size();i++)
		{
			for(int j=0;j<sampling_table[i].size();j++)
			{
				out<<sampling_table[i][j]<<" ";
			}
			out<<endl;
		}
	}
	
};
#endif