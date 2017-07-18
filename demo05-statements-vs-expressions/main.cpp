
#include <iostream>
#include <cstdio>
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
  std::cout << "Now that we are familiar with the >> operator in Haskell, we should look for similar patterns elsewhere" << std::endl;
  std::cout << "For " << "example, " << "the " << "cascading " << "behavior " << "of " << "the " << "<< " << "operator " << "in " << "c++ " << "should " << "seem " << "similar." << std::endl;

 (((((((((((((((std::cout  << "Thought ") << "admittedly ") << "it ") << "is ") << "not ") << "*as* ") << "associative ") << "as ") << "haskell, ") << "these ") << "parens ") << "cannot ") << "be ") << "re-ordered ") << std::endl);

 
}

///////////////////////////////////////////////////////////////////////////////
// Main 2

void main2() {
  std::cout << "Now we get to the challenge of this chapter. Expressions vs Statements.  Expressions can be " << "*composed*" << " with other expressions, with operators like + or <<, or by function calls." << std::endl;

  std::cout << "But statements cannot. There is exactly one relationship between statements: \"And then...\"" << std::endl;
  
  std::cout << "This limits statements interactions to variables. For example, type something here: " << std::endl;
  std::string user_input;
  std::cin >> user_input;

  std::cout << "You entered \"" << user_input << "\"." << std::endl;

}


///////////////////////////////////////////////////////////////////////////////
// Main 3

class Statement {
public:
  Statement(std::function<int(int)> func) {
    func_ = func;
  }

  int RunStatement(int val) const {
    int ret;
    ret = func_(val);
    return ret;
  }
private:
  std::function<int(int)> func_;
};

Statement operator+(const Statement LHS, const Statement RHS) {
  return Statement([LHS, RHS](int x){
    int temp, ret;
    temp = LHS.RunStatement(x);
    ret = RHS.RunStatement(temp);
    return ret;
  });
}

void main3() {
  
  std::cout << "New riddle, can we expend our ThingThatCanBeDone so that it can communicate a variable from one Thing to the next Thing?" << std::endl;
  Statement my_statement = 
    Statement([](int _) { return 5; }) + 
    Statement([](int x) { std::cout << "Started out with " << x << ", will return " << 2 * x << std::endl; return 2 * x; }) +
    Statement([](int y) { std::cout << "Got " << y << std::endl; return 0; });

  my_statement.RunStatement(0);

}

///////////////////////////////////////////////////////////////////////////////
// Main 4

void main4() {
  std::cout << "This gets us really really close to the real definition of Monads:" << std::endl;
  std::cout << "A monad is a monoid in the category of endofunctors" << std::endl;
  std::cout << "Lets map that to something more familiar" << std::endl;
  std::cout << "A statement is part of a sequence of linked functions" << std::endl;
}

