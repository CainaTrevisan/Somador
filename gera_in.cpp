#include <iostream>
#include <string>
#include <random>
#include <cfloat>
#include <cmath>
#include <vector>

using namespace std;

string floatToBinary(float f)
{
	string str;
	union { float f; uint32_t i; } u;
	u.f = f;

	for (int i = 0; i < 32; i++)
	{
		if (u.i % 2)  str.push_back('1');
		else str.push_back('0');
		u.i >>= 1;
	}

	// Reverse the string since now it's backwards
	string temp(str.rbegin(), str.rend());
	str = temp;
	return str;
}

string binToHex(const string &entrada)
{
    vector<pair<string,char>> hexCode{
      {"0000", '0'},
      {"0001", '1'},
      {"0010", '2'},
      {"0011", '3'},
      {"0100", '4'},
      {"0101", '5'},
      {"0110", '6'},
      {"0111", '7'},
      {"1000", '8'},
      {"1001", '9'},
      {"1010", 'a'},
      {"1011", 'b'},
      {"1100", 'c'},
      {"1101", 'd'},
      {"1110", 'e'},
      {"1111", 'f'}
    };

    string palavra;
    string hex;
    for(auto i=0; i<32; i+=4)
    {
        palavra.clear();
        for(auto j=0; j<4; ++j)
            palavra.push_back(entrada[i+j]);

        for(auto i : hexCode){
            if (i.first==palavra)
                hex.push_back(i.second);

        }

    }
    return hex;
}

int main() {

	random_device rd;
    mt19937 e2(rd());
    lognormal_distribution<float> log_dist23(0.0,23.0);
    lognormal_distribution<float> log_dist231(0.0,93.0);
	uniform_int_distribution<int> dist_sinal(0,1);
	uniform_real_distribution<float> dist1(0,1);
	uniform_real_distribution<float> distden(0.000000000000000000000000000000000000005877472,0.000000000000000000000000000000000000001469368);
	uniform_real_distribution<float> dist10(-10.0,10.0);
	uniform_real_distribution<float> dist100(-100.0,100.0);
	uniform_real_distribution<float> dist4(-4,4);
	uniform_real_distribution<float> real_dist(FLT_MIN,FLT_MAX);
	uniform_real_distribution<float> distdenormal(-0.000000000000000000000000000000000000011754942,0.000000000000000000000000000000000000011754942);
	int a=200;

    cout << "int A[] = {\n";

	while(--a) {

		int sinal1 = (dist_sinal(e2)) ? 1 : -1; 
		int sinal2 = (dist_sinal(e2)) ? 1 : -1; 

		float float1 = sinal1 * distdenormal(e2);
		float float2 = sinal2 * distden(e2);

//		cout << "(b\"" << floatToBinary(float1) << "\", b\"" << floatToBinary(0.0) << "\"),\n";
//		cout << "(b\"" << floatToBinary(float2) << "\", b\"" << floatToBinary(float1+float2) << 
//			"\"), -- " << float1 << " + " << float2 << " = " << float1+float2 << "\n";

        cout << "    (int)0x" << binToHex(floatToBinary(float1)) << ",(int)0x" <<
                binToHex(floatToBinary(float2)) << ",(int)0x" <<
                binToHex(floatToBinary(float1+float2));

        if (a!=1)
            cout << ",\n";

	}
    cout << "\n};";
}
