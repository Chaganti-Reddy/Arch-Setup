#include <bits/stdc++.h>
using namespace std;

int main() {
  // your code goes here
  int t;
  cin >> t;
  while (t--) {
    int n, h = 0, t = 0, ans = 0, H = 0, T = 0;
    cin >> n;
    string s;
    cin >> s;
    s.erase(remove(s.begin(), s.end(), '.'), s.end());

    for (int i = 0; i < s.size(); ++i) {
      if (s[i] == 'H')
        H++;
      else if (s[i] == 'T')
        T++;
    }

    if (H == T) {
      for (int i = 0; i < s.size(); ++i) {
        if (s[0] == 'T') {
          ans = 0;
          break;
        } else if (s[i] == 'H' && h == 0) {
          h = 1;
          t = 0;
          ans = 1;
        } else if (s[i] == 'H' && h == 1) {
          ans = 0;
          break;
        } else if (s[i] == 'T' && t == 0) {
          t = 1;
          h = 0;
          ans = 1;
        } else if (s[i] == 'T' && t == 1) {
          ans = 0;
          break;
        }
      }
      if (ans == 1 || s.size() == 0 && h == 0)
        cout << "Valid" << endl;
      else
        cout << "Invalid" << endl;
    } else {
      cout << "Invalid" << endl;
    }
  }
  return 0;
}
