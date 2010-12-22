#include "agpl__utils_c++.h"
#include <sstream>
#include <string>

namespace agpl {

    namespace utils {

        using namespace std;

        string itoa (int i) {
            stringstream s;
            s << i;
            return string(s.str());
        }

    }

}
