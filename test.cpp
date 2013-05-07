#include <map>
#include <vector>
#include <string>
#include <iostream>

using std::vector;
using std::string;
using std::map;
using std::cerr;

struct defined
{
  enum Type{
    OBJ_LIKE,
    FUNC_LIKE,
  };
  const Type t;
  vector<string> macro_body;
  vector<string> param_list;
  defined(Type t):t(t)
  {
  }
  void push_macro(const char* s)
  {
    macro_body.push_back(s);
  }
  void push_macro(string& s)
  {
    macro_body.push_back(s);
  }
  void push_param(string& s)
  {
    param_list.push_back(s);
  }
  ~defined()
  {
    macro_body.clear();
    param_list.clear();
  }
};

void warning(string s)
{
  std::cerr << s << std::endl;
}
struct macro_defined
{
  typedef map<string, defined> macros_type;
  macros_type macros_map;
  
  void push_defined(string& s, defined& d)
  {
    // if redefined
    if(macros_map.find(s) != macros_map.end())
      warning("redefined " + s);
    macros_map.insert(macros_type::value_type(s, d));
  }
  
  void push_defined(string s)
  {
    push_defined(s, "1");
  }
  
  void push_defined(string s, string v)
  {
    defined d(defined::OBJ_LIKE);
    d.push_macro(v);
    push_defined(s, d);
  }
  
  string get_defined(string s)
  {
    string r;
    return macros_map.find(s);
  }
  
  bool has_defined(string s)
  {
    return macros_map.find(s) != macros_map.end();
  }
};

macro_defined g_macros;
int main(int argc, char ** argv)
{
  g_macros.push_defined("X");
  g_macros.push_defined("Y", "hello world");
}
