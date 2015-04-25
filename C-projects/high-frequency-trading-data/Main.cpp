/*
Copyright (c) Ming Tian 2015 All Right Reserved
Date: April, 11 2015
Author: Ming Tian (NYU-Poly)
Topic:FRE-7831-Big Data-Homework2
*/

#include <iterator>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <map>
#include <time.h>
#include <stdlib.h>

using namespace std;

//The .csv loading function is from Stackoverflow Loki Astari
//Available at: http://stackoverflow.com/questions/1120140/how-can-i-read-and-parse-csv-files-in-c/1120224#1120224
//Modified by Ming Tian based on the original function

//Define a CSVRow class for read and parse csv data
class CSVRow
{
public:
	string const& operator[](size_t index) const
	{
		return m_data[index];
	}
	size_t size() const
	{
		return m_data.size();
	}
	void readNextRow(istream& str)
	{
		string         line;
		getline(str, line);

		stringstream   lineStream(line);
		string         cell;

		m_data.clear();
		while (getline(lineStream, cell, ','))
		{
			m_data.push_back(cell);
		}
	}
private:
	vector<string>    m_data;
};

istream& operator>>(istream& str, CSVRow& data)
{
	data.readNextRow(str);
	return str;
}
//CSV loader end here

//define a function to parse day number from the date string
int parsed(string& datestr)
{
	//define a tmp string variable
	string s_tmp = datestr;

	//parsing string
	string delimiter = "-";
	string day = s_tmp.substr(0, s_tmp.find(delimiter));
	//part below is useless now
	/*string left_tmp = s_tmp.erase(0, s_tmp.find(delimiter) + delimiter.length());
	string month = left_tmp.substr(0, left_tmp.find(delimiter));
	string left_tmp = left_tmp.erase(0, left_tmp.find(delimiter) + delimiter.length());
	string year = left_tmp;*/

	//Then convert string to int
	long day_n = atol(day.c_str());
	int day_new = day_n;

	//return result
	return day_new;
}

//define a function for converting time/date string to long/int
long ttol(string& timestr)
{
	//define a tmp string variable
	string s_tmp = timestr;

	//parsing string
	string delimiter = ":";
	string hour = s_tmp.substr(0, s_tmp.find(delimiter));
	string left_tmp = s_tmp.erase(0, s_tmp.find(delimiter) + delimiter.length());
	string min = left_tmp.substr(0, left_tmp.find(delimiter));
	left_tmp = left_tmp.erase(0, left_tmp.find(delimiter) + delimiter.length());

	//jude whether more character exist after sec part
	if (left_tmp.size() > 2)
	{
		//update the delimiter
		string delimiter_new = ".";
		string sec = left_tmp.substr(0, left_tmp.find(delimiter_new));
		//connect the substr
		string conn = hour + min + sec;
		//Then convert string to long
		long ltime = atol(conn.c_str());

		//return reuslt
		return ltime;
	}
	else
	{
		string sec = left_tmp;
		//connect the substr
		string conn = hour + min + sec;

		//Then convert string to long
		long ltime = atol(conn.c_str());

		//return reuslt
		return ltime;
	}
}

//define a function for converting long to time string
string ltostr(long &longtime)
{
	string result_tmp;
	//check whether the long is smaller than 100000
	if (longtime < 100000)
	{
		//string number;
		stringstream strstream;
		strstream << longtime;
		strstream >> result_tmp;
		result_tmp = "0" + result_tmp;
	}
	else
	{
		stringstream strstream;
		strstream << longtime;
		strstream >> result_tmp;
	}

	//parsing string
	string hour = result_tmp.substr(0, 2);
	string min = result_tmp.substr(2, 2);
	string sec = result_tmp.substr(4, 2);
	//connect substrs
	string timestr = hour + ":" + min + ":" + sec;

	//return result
	return timestr;
}


//In the csv data
//The 1 col is Date
//The 2 col is Time
//The 3 col is Timezone
//The 4 col is Quote/Trade
//The 5 col is Price
//The 6 col is Volume
//Other cols are useless for us

//main function
int main()
{
	//here we add a clock to test the total run time of the program
	time_t t1;
	time(&t1);  //first clock has been set up
	cout << "Start running, please wait patiently." << endl;

	//code start from here
	//use the function from csvloader.h
	ifstream       file("SPY_May_2012.csv");
	CSVRow         row;
	//define a map to contain the result
	//The key of the map change from string to long to speed up the program
	//at the output stage, we convert the long key to string type
	map <long, pair<long, double> > mymap;
	//map <long, pair<long, double> >::iterator it;
	//initialize the map key to time intervals
	mymap[93000] = make_pair(0, 0);
	mymap[94500] = make_pair(0, 0);
	mymap[100000] = make_pair(0, 0);
	mymap[101500] = make_pair(0, 0);
	mymap[103000] = make_pair(0, 0);
	mymap[104500] = make_pair(0, 0);
	mymap[110000] = make_pair(0, 0);
	mymap[111500] = make_pair(0, 0);
	mymap[113000] = make_pair(0, 0);
	mymap[114500] = make_pair(0, 0);
	mymap[120000] = make_pair(0, 0);
	mymap[121500] = make_pair(0, 0);
	mymap[123000] = make_pair(0, 0);
	mymap[124500] = make_pair(0, 0);
	mymap[130000] = make_pair(0, 0);
	mymap[131500] = make_pair(0, 0);
	mymap[133000] = make_pair(0, 0);
	mymap[134500] = make_pair(0, 0);
	mymap[140000] = make_pair(0, 0);
	mymap[141500] = make_pair(0, 0);
	mymap[143000] = make_pair(0, 0);
	mymap[144500] = make_pair(0, 0);
	mymap[150000] = make_pair(0, 0);
	mymap[151500] = make_pair(0, 0);
	mymap[153000] = make_pair(0, 0);
	mymap[154500] = make_pair(0, 0);
	mymap[160000] = make_pair(0, 0);

	//start to read the csv file
	//initialize the progress checker
	//double pcheck = 0;
	while (file >> row)
	{

		//firstly, need to judge the date
		string date_tmp = row[1];
		int day_today;
		if (row[1] != "Date[G]")
		{
			day_today = parsed(date_tmp);
		}

		//stop loop if day > 21
		if (day_today == 22)
		{
			break;
		}

		//if day less than 21 (1-20), then accumulate volume in every time intervals
		if (day_today < 21)
		{
			if (row[4] == "Trade")
			{
				//check the value of volume
				//if the value is Null, assign them to 0
				long vol_tmp = 0;
				if (row[6].empty())
				{
					//cout << "warning: this volume is missing, assigned to 0!" << endl;
					vol_tmp = 0;
				}
				else
				{
					string tmp = row[6];
					vol_tmp = atol(tmp.c_str());
				}

				//now we need to check the time in which interval
				string time_tmp = row[2];
				long time_now;
				time_now = ttol(time_tmp);
				//we use iterator here
				for (map <long, pair<long, double> >::iterator it = mymap.begin(); it != mymap.end(); ++it)
				{
					//define the start and end of an interval
					long start = it->first;
					long end = (++it)->first;
					it--;
					if (time_now >= start && time_now < end)
					{
						long volume = it->second.first;
						volume += vol_tmp;
						it->second.first = volume;
						//to speed up, we add a check here
						break;
					}
				}

			}
		}

		//if day equals 21, then extract first trade price in every time intervals
		if (day_today == 21)
		{
			if (row[4] == "Trade")
			{
				//check the value of price
				//if the value is Null, assign them to 0
				double price_tmp = 0;
				if (row[5].empty())
				{
					//cout << "warning: this price is missing, assigned to 0!" << endl;
					price_tmp = 0;
				}
				else
				{
					string tmp = row[5];
					price_tmp = atof(tmp.c_str());
				}

				//now we need to check the time in whicmh interval
				string time_tmp = row[2];
				long time_now;
				time_now = ttol(time_tmp);
				//we use iterator here
				for (map <long, pair<long, double> >::iterator it = mymap.begin(); it != mymap.end(); ++it)
				{
					//define the start and end of an interval
					long start = it->first;
					long end = (++it)->first;
					it--;
					if (time_now >= start && time_now < end)
					{
						//double can not compare with 0, so convert to int
						int second = (int)it->second.second;
						if (second == 0)
						{
							it->second.second = price_tmp;
							//to speed up, we add a check here
							break;
						}
					}
				}
			}
		}
		//add a simple equation here to check the progress rate
		//pcheck += 1;
		//cout << pcheck / 32000000 << endl;

	}

	//save the data in map into a file
	cout << "All data done! Now saving map to a file, please wait." << endl;
	ofstream fout;
	fout.open("map_result.txt");
	for (map <long, pair<long, double> >::iterator it = mymap.begin(); it != mymap.find(160000); ++it)
	{
		long tmp = it->first;
		string key_tmp = ltostr(tmp);
		fout << key_tmp << "," << it->second.first / 14 << "," << it->second.second << "\n";
	}
	fout.close();

	//the second clock
	time_t t2;
	time(&t2);
	double seconds;
	seconds = difftime(t2, t1);
	double min = seconds / 60;
	cout << "The total run time of this program is: " << min << "minutes" << endl;
	system("pause");
	return 0;
}
