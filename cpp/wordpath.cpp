#include <stdio.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <utility>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

typedef vector<int> vi;

void printpath(vector<string>& words, vi& span, int s, int v) {
	cout << words[v] << endl;
	if (s == v) return;
	printpath(words, span, s, span[v]);
}

int main(int argc, char* argv[]) {
	int i, j;
	vector<string>::iterator it;
	if (argc != 3) {
		printf("Usage: ./wordpath <from> <to>\n");
		return -1;
	}
	string wordfrom(argv[1]);
	string wordto(argv[2]);
	if (wordfrom.length() != wordto.length()) {
		printf("Words must have equal length\n");
		return -1;
	}
	int wordlen = wordfrom.length();
	ifstream wordfile("dict.txt");
	map<string, set<string> > wordgraph;
	vector<string> words;
	vector<vi> graph;
	string current;
	while (!wordfile.eof()) {
		getline(wordfile, current);
		if (current.length() != wordlen) continue;
		bool bad = false;
		for (i = 0; i < wordlen; i++) {
			if (current[i] < 'a' || current[i] > 'z') bad = true;
		}
		if (bad) continue;
		int curindex = words.size();
		words.push_back(current);
		vi adj;
		graph.push_back(adj);
		for (i = 0; i < wordlen; i++) {
			string modword(current);
			char sub;
			for (sub = 'a'; sub <= 'z'; sub++) {
				modword[i] = sub;
				if (!modword.compare(current)) continue;
				it = lower_bound(words.begin(), words.end(), modword);
				if (it != words.end() && !it->compare(modword)) {
					int modindex = distance(words.begin(), it);
					graph[curindex].push_back(modindex);
					graph[modindex].push_back(curindex);
				}
			}
		}
	}
	it = lower_bound(words.begin(), words.end(), wordfrom);
	if (it == words.end() || it->compare(wordfrom)) {
		printf("Not all of your words are real.\n");
		return -1;
	}
	int fromindex = distance(words.begin(), it);
	it = lower_bound(words.begin(), words.end(), wordto);
	if (it == words.end() || it->compare(wordto)) {
		printf("Not all of your words are real.\n");
		return -1;
	}
	int toindex = distance(words.begin(), it);
	int nWords = words.size();
	vi span(nWords);
	span.assign(nWords, -1);
	span[toindex] = -2;
	queue<int> pending;
	pending.push(toindex);
	while (!pending.empty()) {
		int fromcursor = pending.front(); pending.pop();
		if (fromcursor == fromindex) break;
		for (i = 0; i < graph[fromcursor].size(); i++) {
			int tocursor = graph[fromcursor][i];
			if (span[tocursor] != -1) continue;
			pending.push(tocursor);
			span[tocursor] = fromcursor;
		}
	}
	if (span[fromindex] == -1) {
		cout << "There is no path from " << wordfrom << " to " << wordto << "." << endl;
		return 0;
	}
	printpath(words, span, toindex, fromindex);
}
