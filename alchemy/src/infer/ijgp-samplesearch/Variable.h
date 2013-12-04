#ifndef VARIABLE_H_
#define VARIABLE_H_
#include <vector>
#include <cassert>
#include "Util.h"
#include <gmp.h>
#include <gmpxx.h>
#include <map>
using namespace std;

typedef mpz_class Int;
struct Variable
{
public:
	int id_;
	vector<int> domain_;
	int value_;
	int addr_value_;
public:
	vector<int> old_domain;
	int orig_id;
	vector<int> mapping;
	int temp_value;
	Variable():value_(INVALID_VALUE),id_(INVALID_VALUE),addr_value_(INVALID_VALUE){}
	Variable(const int& id,const vector<int>& domain): id_(id),domain_(domain),addr_value_(INVALID_VALUE),value_(INVALID_VALUE), old_domain(domain),mapping(domain)
	{
	}
	~Variable(){}
	//Variable(const int& id, const int& domain_size_): id_(id),domain_(vector<int> (domain_size_)),addr_value_(INVALID_VALUE),value_(INVALID_VALUE),old_domain(domain_),mapping(domain_)
	//{
	//}
	// Give access to private members
	vector<int>& domain() { return domain_;}
	int& id()  {return id_;}
	int& addr_value()  { return (value_==INVALID_VALUE)?(addr_value_):(value_);}
	int& value()  { return value_;}

	int id() const { return id_;}
	void print(ostream& out=cout) { cout<<"Var id = "<<id_<<" domain size = "<<domain_size()<<endl;}

	/*
	const vector<int>& domain() const { return domain_;}
	const int id() const { return id_;}
	const int addr_value() const  { return addr_value_;}
	const int value()  const { return value_;}
	*/
	const int domain_size() const  { return (int)domain_.size();}

	void updateDomain(vector<bool>& new_dom)
	{
		assert(new_dom.size() == domain_.size());
		old_domain=domain_;
		mapping=vector<int>();
		int count=0;
		domain_=vector<int>();
		for(int i=0;i<new_dom.size();i++)
		{
			if(new_dom[i])
			{
				domain_.push_back(count);
				mapping.push_back(i);
				count++;
			}
		}
		if(domain_.empty())
			cerr<<"Something wrong\n";
	}
	inline static int getValueAddress(const vector<Variable*>& variables)
	{
		int add_ress=0;
		int multiplier=1;
		for(int i=0;i<variables.size();i++)
		{
			assert(variables[i]->value()!=INVALID_VALUE);
			add_ress+=(multiplier*variables[i]->value());
			multiplier*=variables[i]->domain_size();
		}
		return add_ress;
	}
	// Convert assignment to address...Example: The assignment (A1=0,A2=1) is equivalent to the address 1
	inline static int getAddress (const vector<Variable*>& variables)
	{
		int add_ress=0;
		int multiplier=1;
		for(int i=0;i<variables.size();i++)
		{
			add_ress+=(multiplier*variables[i]->addr_value());
			multiplier*=variables[i]->domain_size();
		}
		return add_ress;
	}
	static void getAddressBigInt(const vector<Variable*>& variables,Int& add_ress_)
	{
		//mpz_t add_ress;
		mpz_t add_ress;
		mpz_init(add_ress);
		mpz_t multiplier;
		//mpz_init(multiplier);
		mpz_init_set_si(multiplier,(long int)1);
		for(int i=0;i<variables.size();i++)
		{
			mpz_t temp_big_int;
			mpz_init_set(temp_big_int,multiplier);
			mpz_mul_si(temp_big_int,temp_big_int,(long int) variables[i]->addr_value());
			mpz_add(add_ress,add_ress,temp_big_int);
			mpz_mul_si(multiplier,multiplier,(long int)variables[i]->domain_size());
			mpz_clear(temp_big_int);
			//add_ress+=(multiplier*variables[i]->addr_value());
			//multiplier*=variables[i]->domain_size();
		}
		//return add_ress;
		add_ress_=Int(add_ress);
		mpz_clear(multiplier);
		mpz_clear(add_ress);
	}
	// Convert address to assignment
	inline static void setAddress(const vector<Variable*>& variables, const int add_ress_)
	{
		
		int add_ress=add_ress_;
		for(int i=0;i<variables.size();i++)
		{
			assert(variables[i]->value()==INVALID_VALUE);
			variables[i]->addr_value()=add_ress%variables[i]->domain_size();
			add_ress/=variables[i]->domain_size();
		}	
	}
	
	static void setAddressBigInt(const vector<Variable*>& variables, const Int& add_ress_)
	{
		//int add_ress=add_ress_;
		mpz_t add_ress;
		mpz_init_set(add_ress,add_ress_.get_mpz_t());
		for(int i=0;i<variables.size();i++)
		{
			assert(variables[i]->value()==INVALID_VALUE);
			//variables[i]->addr_value()=add_ress%variables[i]->domain_size();
			//add_ress/=variables[i]->domain_size();
			mpz_t r;//,q;
			mpz_init(r);
			//mpz_init(q);
			//mpz_mod_ui(r,add_ress,(unsigned long int)variables[i]->domain_size());
			
			mpz_fdiv_qr_ui(add_ress,r,add_ress,variables[i]->domain_size());
			variables[i]->addr_value()=mpz_get_si(r);
			mpz_clear(r);
		}
		mpz_clear(add_ress);
	}
	static void setAddressVIB(const vector<Variable*>& variables, const int add_ress_)
	{
		int add_ress=add_ress_;
		for(int i=variables.size()-1;i>-1;i--)
		{
			
			variables[i]->addr_value()=add_ress%variables[i]->domain_size();
			add_ress/=variables[i]->domain_size();
		}	
		
	}
	// Get the maximum domain size of the set of variables
	static int getDomainSize(const vector<Variable*>& variables)
	{
		int domain_size=1;
		for(int i=0;i<variables.size();i++)
			domain_size*=variables[i]->domain_size();
		return domain_size;
	}
	
};
bool less_than_comparator_variable(const Variable* a, const Variable* b);
#endif
