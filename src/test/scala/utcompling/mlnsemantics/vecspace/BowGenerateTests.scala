package utcompling.mlnsemantics.vecspace

class BowGenerateTests {

  // Input: 

  //	the dog walks ; the cat walks quietly .
  //	the dog walks .
  //	the dog walks quickly .
  //	the cat sleeps .
  //	the dog sleeps .

  // Output:

  //	cat						dog	0.68					quietly	2.09	sleeps	3.75	the	1.38	walks	0.77
  //	dog			cat	0.51				quickly	2.09	quietly	0.77	sleeps	1.38	the	1.38	walks	1.38
  //	quickly					dog	1.86													the	1.38	walks	2.09
  //	quietly		cat	3.75	dog	1.86													the	1.38	walks	2.09
  //	sleeps		cat	1.38	dog	0.68													the	1.38		
  //	the			cat	1.38	dog	1.43	quickly	1.16	quietly	1.16	sleeps	2.09	the	0.28	walks	1.16
  //	walks		cat	1.38	dog	1.86	quickly	2.09	quietly	2.09					the	1.38	walks	0.77

}
