#include <bits/stdc++.h>
#include <cstdlib>
using namespace std;

int bookingNo = 0;

class Ticket {
public:
  float adult_1 = 200;
  float adult_2 = 300;
  float child_1 = 120;
  float child_2 = 180;
  float senior_1 = 160;
  float senior_2 = 240;
  // 2 adults and 3 children
  float family_1 = 600;
  float family_2 = 900;
  // group of six or morev the price is per person
  float six_1 = 150;
  float six_2 = 220.5;

  // lion feeding
  float lion = 20.5;
  // penguin feeding
  float penguin = 20;
  // evening Barbeque on on two day ticket
  float Barbeque = 50;

  float noOfAdults, noOfChilds, noOfSeniors, days, date;
  bool lionFeedNeeded = false, penguinFeedNeeded = false,
       barbequeNeeded = false;

  int adult, senior, child, family, six;

  float value, before;
  float totalPeoples;
  // finding the bill
  int total() {
    totalPeoples = noOfAdults + noOfChilds + noOfSeniors;
    if (days == 1) {
      value = adult * adult_1 + child * child_1 + family * family_1 +
              senior * senior_1 + six * six_1;
      before = value;
      cout << "Your total till now is : " << value << "\n";
      if ((int(noOfAdults + noOfSeniors) % 2 == 0) &&
          (int(noOfChilds) % 3 == 0)) {
        cout << "\nA family Ticket is Better for You!" << endl;
        string takefamily;
        cout << "Book the family ticket?(yes or no) ";
        cin >> takefamily;
        if (takefamily == "yes") {
          value = ((totalPeoples / 5) * family_1);
          cout << "Your total value without extras is : " << value << "\n";
        }
      } else {
        cout << "A Group Ticket is Better for You!" << endl;
        string takegroup;
        cout << "Book the group ticket?(yes or no) ";
        cin >> takegroup;
        if (takegroup == "yes") {
          value = min(((noOfAdults * adult_1) + (noOfChilds * child_1) +
                       (noOfSeniors * senior_1)),
                      (totalPeoples * six_1));
          cout << "Your total value without extras is : " << value << "\n";
        }
      }
    }
    if (days == 2) {
      value = adult * adult_2 + child * child_2 + family * family_2 +
              senior * senior_2 + six * six_2;
      before = value;
      cout << "Your total till now is : " << value << "\n";
      if ((int(noOfAdults + noOfSeniors) % 2 == 0) &&
          (int(noOfChilds) % 3 == 0)) {
        cout << "\nA family Ticket is Better for You!" << endl;
        string takefamily;
        cout << "Book the family ticket?(yes or no) " << endl;
        cin >> takefamily;
        if (takefamily == "yes") {
          value = ((totalPeoples / 5) * family_2);
          cout << "Your total value without extras is : " << value << "\n";
        }
      } else {
        cout << "\nA Group Ticket is Better for You!";
        string takegroup;
        cout << "\nBook the group ticket?(yes or no) " << endl;
        cin >> takegroup;
        if (takegroup == "yes") {
          value = min(((noOfAdults * adult_2) + (noOfChilds * child_2) +
                       (noOfSeniors * senior_2)),
                      (totalPeoples * six_2));
          cout << "Your total value without extras is : " << value << "\n";
        }
      }
    }

    return value;
  }
};

void book_ticket() {
  Ticket first;
  cout << "Welcome!\n";
  cout << "Here are the various options we Provide" << endl;
  cout << "---------------------------------For One "
          "day---------------------------------\n\n";
  cout << ">>>Options\n\n";
  cout << "Persons\t\t\t\t\t\t\t\tCost\n";
  cout << "1 Adult"
       << "\t\t\t\t\t\t\t\t" << first.adult_1 << endl;
  cout << "1 Child"
       << "\t\t\t\t\t\t\t\t" << first.child_1 << endl;
  cout << "1 Senior"
       << "\t\t\t\t\t\t\t" << first.senior_1 << endl;
  cout << "1 family(up to two adults or seniors, and three children)"
       << "\t" << first.family_1 << endl;
  cout << "groups of six people or more, price per person"
       << "\t\t\t" << first.six_1 << endl;
  cout << "\n";

  cout << ">>>Attractions\n\n";
  cout << "Attraction\t\t\t\t\t\t\t\tCost Per person\n";
  cout << "Lion Feeding"
       << "\t\t\t\t\t\t\t\t" << first.lion << endl;
  cout << "Penguin Feeding"
       << "\t\t\t\t\t\t\t\t" << first.penguin << endl;

  cout << "\n---------------------------------For Two "
          "day---------------------------------\n\n";
  cout << ">>>Options\n\n";
  cout << "Persons\t\t\t\t\t\t\t\tCost\n";
  cout << "1 Adult"
       << "\t\t\t\t\t\t\t\t" << first.adult_2 << endl;
  cout << "1 Child"
       << "\t\t\t\t\t\t\t\t" << first.child_2 << endl;
  cout << "1 Senior"
       << "\t\t\t\t\t\t\t" << first.senior_2 << endl;
  cout << "1 family(up to two adults or seniors, and three children)"
       << "\t" << first.family_2 << endl;
  cout << "groups of six people or more, price per person"
       << "\t\t\t" << first.six_2 << endl;
  cout << "\n";

  cout << ">>>Attractions\n\n";
  cout << "Attraction\t\t\t\t\t\t\t\tCost Per person\n";
  cout << "Lion Feeding"
       << "\t\t\t\t\t\t\t\t" << first.lion << endl;
  cout << "Penguin Feeding"
       << "\t\t\t\t\t\t\t\t" << first.penguin << endl;
  cout << "Evening Barbecue(only in two day ticket)"
       << "\t\t\t\t" << first.Barbeque << endl;

  cout << "\n\n";
  cout << "Days available for Booking\n\n";

  time_t t;
  t = time(NULL);
  struct tm tm = *localtime(&t);
  for (int i = 0; i < 7; i++) {
    cout << tm.tm_mday + i << "-" << tm.tm_mon + 1 << "-" << tm.tm_year + 1900
         << endl;
  }

  cout << "\n------------------------------------------------------------------"
          "------\n";

  cout << "\nEnter the no. of Adults : ";
  cin >> first.noOfAdults;
  cout << "\nEnter the no. of Children :";
  cin >> first.noOfChilds;

  if (first.noOfChilds > 2 * first.noOfAdults) {
    cout << "Not allowed only two childs allowed per adult" << endl;
    cout << "\nEnter the no. of Children :";
    cin >> first.noOfChilds;
  }

  cout << "\nEnter the no. of Senior Citizens : ";
  cin >> first.noOfSeniors;
  cout << "\n";

  cout << "\nEnter the no. of days you want to book?(only 1 or 2) : ";
  cin >> first.days;
  cout << "\nEnter the date you want to book? (Between" << tm.tm_mday << " to "
       << tm.tm_mday + 6 << ": ";
  cin >> first.date;
  if (first.date > tm.tm_mday + 6 || first.date < tm.tm_mday) {
    cout << "Not allowed that day select between ( " << tm.tm_mday << " to "
         << tm.tm_mday + 6 << " : ";
    cin >> first.date;
  }

  cout << "\nEnter the no. of Adult Ticket wanted : ";
  cin >> first.adult;
  cout << "\nEnter the no. of child Ticket wanted:";
  cin >> first.child;
  cout << "\nEnter the no. of Senior Tickets wanted : ";
  cin >> first.senior;
  cout << "\nEnter the no. of family Tickets wanted : ";
  cin >> first.family;
  cout << "\nEnter the no. of group of six Tickets wanted : ";
  cin >> first.six;
  cout << "\n";

  first.total();

  cout << "\nNow choose the extras... \n";
  string lionfeedvalue;
  cout << "\nWant to include Lion Feeding?(yes or no) ";
  cin >> lionfeedvalue;
  if (lionfeedvalue == "yes") {
    first.lionFeedNeeded = true;
  }

  string penguinFeedValue;
  cout << "\nWant to include Penguin Feeding?(yes or no) ";
  cin >> penguinFeedValue;
  if (penguinFeedValue == "yes") {
    first.penguinFeedNeeded = true;
  }
  if (first.days == 2) {
    string barbaqueNeededValue;
    cout << "\nWant to include barbeque?(yes or no) ";
    cin >> barbaqueNeededValue;

    if (barbaqueNeededValue == "yes") {
      first.barbequeNeeded = true;
    }
  }

  if (first.lionFeedNeeded) {
    first.value += first.lion * first.totalPeoples;
    first.before += first.lion * first.totalPeoples;
  }
  if (first.penguinFeedNeeded) {
    first.value += first.penguin * first.totalPeoples;
    first.before += first.penguin * first.totalPeoples;
  }
  if (first.barbequeNeeded) {
    first.value += first.Barbeque * first.totalPeoples;
    first.before += first.Barbeque * first.totalPeoples;
  }

  string confirm;
  cout << "\nPlease confirm that can we book the ticket for you?(yes/no) : ";
  cin >> confirm;
  if (confirm == "yes") {
    bookingNo++;
    cout << "\nyour ticket is successfully booked ..... \n";
    cout << "\nTotal Cost including extras is :" << first.value << endl;
    cout << "You saved : " << first.before - first.value << endl;
    cout << "Your Booking no. is " << bookingNo << endl;
    cout << "Booking date is " << first.date << endl;
    cout << "Booking done for " << first.noOfAdults << " adults "
         << first.noOfSeniors << " Senior Citizens " << first.noOfChilds
         << " Childs \n\n";
  } else {
    exit(1);
  }
}

int main() {
  int count = 0;
  while (1) {
    cout << "----------------------------------------- WELCOME TO ZOO TICKET "
            "BOOKING "
            "-----------------------------------------";
    cout << "\n1)Book a ticket\n2)Exit\nEnter your choise(1/2) : ";
    cin >> count;
    switch (count) {
    case 1:
      book_ticket();
      break;
    case 2:
      exit(1);
    }
  }

  return 0;
}
