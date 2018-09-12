
#include <iostream>
#include <vector>
#include <functional>

void main1();
void main2();
void main3();
void main4();

int main() {
  std::cout << "==================== Main 1 ====================" << std::endl;
  main1();
  std::cout << "==================== Main 2 ====================" << std::endl;
  main2();
  std::cout << "==================== Main 3 ====================" << std::endl;
  main3();
  std::cout << "==================== Main 4 ====================" << std::endl;
  main4();
  return 0;
}


///////////////////////////////////////////////////////////////////////////////
// Main 1
void main1() {
  std::cout << "Ints are monids because you can + them." << std::endl;
  int a = 1 + 2 + 3 + 4;
  std::cout << "a = " << a << std::endl;
}

///////////////////////////////////////////////////////////////////////////////
// Main 2
void main2() {
  std::cout << "Strings are also monoids. Note that monoids are associative, but may not be not commutative" << std::endl;
  std::string a = std::string("a") + std::string("b") + std::string("c") + std::string("d");
  std::cout << "a = " << a << std::endl;
}


///////////////////////////////////////////////////////////////////////////////
// Main 3
// We can make std::vector into a Monoid easily enough
template <typename T>
std::vector<T> operator+(std::vector<T> LHS, std::vector<T> RHS) {
  std::vector<T> ret;
  for (auto x: LHS) {
    ret.push_back(x);
  }
  for (auto x: RHS) {
    ret.push_back(x);
  }
  return ret;
}

void main3() {
  std::cout << "Lists or list like things are monoids in most languages too" << std::endl;
  std::cout << "(And it is easy enough to fix C++)" << std::endl;
  std::vector<int> a,b,c;
  a.push_back(1);
  a.push_back(2);
  b.push_back(3);
  b.push_back(4);
  c.push_back(5);
  c.push_back(6);

  std::vector<int> d = a + b + c;
  
  std::cout << "[ ";
  for(auto x : d) {
    std::cout << x << " ";
  }
  std::cout << "]" << std::endl;

}

///////////////////////////////////////////////////////////////////////////////
// Main 4

// Now lets see if we make logic into a monoid, by adding a + operator to functions (kind of)

class ThingThatCanBeDone {
public:
  ThingThatCanBeDone(std::function<void(void)> func) {
    func_ = func;
  }

  void DoThing() const {
    func_();
  }
private:
  std::function<void(void)> func_;
};


// Important note!: This is kind of an implicit linked list (LISP style)
ThingThatCanBeDone operator+ (ThingThatCanBeDone LHS, ThingThatCanBeDone RHS) {
  return ThingThatCanBeDone([LHS, RHS]{
    LHS.DoThing();
    RHS.DoThing();
  });
}

void main4() {
  std::cout << "If lists are Monoids, then lists of functions must be too" << std::endl;
  ThingThatCanBeDone say_hello([]{
    std::cout << "Hello ";
  });
  ThingThatCanBeDone say_monoid([]{
    std::cout << "Monoid ";
  });
  ThingThatCanBeDone say_world([]{
    std::cout << "World ";
  });
  ThingThatCanBeDone say_newline([]{
    std::cout << std::endl;
  });

  ThingThatCanBeDone greet = say_hello + say_monoid + say_world + say_newline;
  
  greet.DoThing();

}

