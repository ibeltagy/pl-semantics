#ifndef SS_LOGFUNCTION_H_
#define SS_LOGFUNCTION_H_

#include <vector>
#include <cmath>
#include "Variable.h"
#include "Double.h"
#include "Function.h"

using namespace std;

namespace ss{

struct LogDouble
{
	long double val;
	//double val;
	bool is_zero;
	LogDouble():val(0.0),is_zero(false){}
	LogDouble(double value):is_zero((value < DBL_MIN)?(true):(false)), val((value < DBL_MIN)? (0):(log(value))){ }
	LogDouble(Double& value):is_zero((value.isZero())?(true):(false)),val((value.isZero())?(0):(log(value.value()))){ }
	~LogDouble(){}
	LogDouble operator + (const LogDouble& other)
	{
		if(is_zero || other.is_zero)
		{
			return LogDouble(0.0);
		}
		return LogDouble(val+other.val);
	}
	LogDouble operator +=(const LogDouble& other)
	{
		if(is_zero || other.is_zero){
			is_zero=true;
			return *this;
		}
		val+=other.val;
		return *this;
	}
	LogDouble operator - (const LogDouble& other)
	{
		if(is_zero || other.is_zero)
		{
			return LogDouble(0.0);
		}
		return LogDouble(val-other.val);
	}
	LogDouble operator -=(const LogDouble& other)
	{
		if(is_zero || other.is_zero)
		{
			is_zero=true;
			return *this;
		}
		val-=other.val;
		return *this;
	}
	Double toDouble(){ return (is_zero) ? (Double()):(Double(exp(val)));}
	double todouble(){ return (is_zero) ? (0.0): (exp(val));}
	long double toLongdouble(){ return ((is_zero)? (0.0): (val));}
};
struct LogFunction: public Function
{
	LogFunction():Function(){}
	//LogFunction(Function& function){variables_=function.variables();table_=function.table(); toLogTable();}
	LogFunction(Function& function);
	~LogFunction() {}
	
	vector<Variable*>& variables()  { return variables_;}
	vector<Double>& table() { return table_;}

	vector<LogDouble> log_table;
	void product(LogFunction& function);
	void toLogTable()
	{
		log_table=vector<LogDouble>(table_.size());
		for(int i=0;i<table_.size();i++)
		{
			log_table[i]=LogDouble(table_[i]);
		}
		table_.clear();
	}
	void print(ostream& out);
	static void multiplyAndMarginalize(vector<Variable*>& marg_variables_,vector<LogFunction*>& functions, LogFunction& out_function,bool normalize=true);
	static void multiplyAndMarginalize(vector<Variable*>& marg_variables_,vector<LogFunction*>& functions, Function& out_function,bool normalize=true);

};
}
#endif
