#include <iostream>
using namespace std;

int main() {
  long long int n, k, count = INT32_MAX;
  cin >> n >> k;
  int a[n];
  for (int i = 0; i < n; i++)
    cin >> a[i];

  for (int i = 0; i < n; i++) {
    count = min(count, abs(a[i] + a[i + 1] - k));
  }
  cout << count << endl;
}
