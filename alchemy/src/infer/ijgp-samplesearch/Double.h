#ifndef DOUBLE_H_
#define DOUBLE_H_

#include <iostream>
#include <cfloat>
#include <cmath>
#include <iomanip>
using namespace std;

#define DBL_MIN3 (LDBL_MIN/(double)2.1)
class Double
{
private:
	bool is_zero;
	long double val;
public:
	Double():
		is_zero(true),
		val(0)
	{
	}
	Double (long double val_):
		is_zero(false),
		val(val_)
	{
		if(val < DBL_MIN3)
			val=DBL_MIN3;
	}
	Double(const Double& term): is_zero(term.isZero()),val(term.value())
	{
	}
	inline bool isZero() const { return is_zero;}
	inline long double value() const {
		if (is_zero)
			return 0.0;
		else	
			return val;
	}
	Double& operator = (const Double& term)
	{
		this->val=term.value();
		this->is_zero=term.isZero();
		return *this;
	}
	Double operator + (const Double& add_term)
	{
		if(is_zero && add_term.isZero())
			return Double();
		if(is_zero)
			return add_term;
		if(add_term.isZero())
			return *this;
		return Double(add_term.value()+val);
	}
	Double& operator +=(const Double& add_term)
	{
		if(is_zero && add_term.isZero())
		{
			//cout<<"Hre\n";
			return *this;
		}
		if(add_term.isZero())
			return *this;
		if(is_zero)
		{
			val=add_term.value();
			is_zero=add_term.isZero();
			return *this;
		}
		val+=add_term.value();
		if(this->is_zero)
			this->is_zero=false;
		return *this;
	}
	Double operator - (const Double& add_term)
	{
		if(is_zero && add_term.isZero())
			return Double();
		if(is_zero)
			return Double(-add_term.value());
		if(add_term.isZero())
			return *this;
		return Double(add_term.value()-val);
	}
	Double operator / (const Double& add_term)
	{
		if(add_term.isZero() || is_zero)
			return Double();
		return Double(this->val/add_term.value());
	}
	Double& operator /= (const Double& add_term)
	{
		if(add_term.isZero() || is_zero)
		{
			is_zero=true;
			val=0.0;
			return *this;
		}
		val/=add_term.value();
		return *this;
	}
	Double operator * (const Double& add_term)
	{
		if(add_term.isZero() || is_zero)
			return Double();
		long double ret_val=this->val*add_term.value();
		if(ret_val < DBL_MIN3)
			ret_val=DBL_MIN3;
		return Double(ret_val);
	}
	Double& operator *= (const Double& add_term)
	{
		if(add_term.isZero() || is_zero)
		{
			is_zero=true;
			val=0.0;
			return *this;
		}
		this->val*=add_term.value();
		if(val < DBL_MIN3)
			val=DBL_MIN3;
		return *this;
	}
	bool operator < (const Double& term)
	{
		if(is_zero && term.isZero())
			return false;
		if(is_zero)
			return true;
		if(term.isZero())
			return false;
		if(val<term.value())
			return true;
		return false;
	}
	bool operator <= (const Double& term)
	{
		if(is_zero && term.isZero())
			return true;
		if(is_zero)
			return true;
		if(term.isZero())
			return false;
		if(val<=term.value())
			return true;
		return false;
	}
	bool operator > (const Double& term)
	{
		if(is_zero && term.isZero())
			return false;
		if(is_zero)
			return false;
		if(term.isZero())
			return true;
		if(val>term.value())
			return true;
		return false;
	}
	bool operator >= (const Double& term)
	{
		if(is_zero && term.isZero())
			return true;
		if(is_zero)
			return false;
		if(term.isZero())
			return true;
		if(val>=term.value())
			return true;
		return false;
	}
	bool operator == (const Double& term)
	{
		if(is_zero && term.isZero())
			return true;
		if(is_zero)
			return false;
		if(term.isZero())
			return false;
		if(fabs(val-term.value())< 0.00000000000000000000000000000000001)
			return true;
		return false;
	}
	friend ostream& operator << (ostream& os, const Double& dob)
	{
		os.setf(ios::fixed,ios::floatfield);
		if(dob.isZero())
			os<<"0.0";
		else
			os<<dob.value();
		return os;
	}
	friend istream& operator >> (istream& is, Double& dob)
	{
		long double value;
		is>>value;
		
		if(value < DBL_MIN)
		{
			dob=Double();
			return is;
		}
		dob=Double(value);
		return is;
	}
};
#endif


/*
void testDouble()
{
	Double temp;
	cout<<"Currently temp is "<<temp<<endl;

	temp=Double(2.0);
	Double temp1(3.333);

	cout<<"temp + temp1 = "<<temp+temp1<<endl;
	cout<<"temp * temp1 = "<<temp*temp1<<endl;
	cout<<"temp / temp1 = "<<temp/temp1<<endl;

	Double temp3=temp;
	cout<<"temp3 = "<<temp3<<endl;

	temp3+=temp1;
	cout<<"temp + temp1 = "<<temp3<<endl;

	temp3=temp;
	temp3*=temp1;
	cout<<"temp * temp1 = "<<temp3<<endl;

	
	temp3=temp;
	temp3/=temp1;
	cout<<"temp / temp1 = "<<temp3<<endl;


	/////////////////
	cout<<"________\n";
	temp1=Double();

	cout<<"temp + temp1 = "<<temp+temp1<<endl;
	cout<<"temp * temp1 = "<<temp*temp1<<endl;
	cout<<"temp / temp1 = "<<temp/temp1<<endl;

	temp3=temp;
	cout<<"temp3 = "<<temp3<<endl;

	temp3+=temp1;
	cout<<"temp + temp1 = "<<temp3<<endl;

	temp3=temp;
	temp3*=temp1;
	cout<<"temp * temp1 = "<<temp3<<endl;

	temp3=temp;
	temp3/=temp1;
	cout<<"temp / temp1 = "<<temp3<<endl;


}
*/
