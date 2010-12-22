#include "agpl__trace_c++.h"

namespace agpl {

    namespace trace {

        extern "C" void agpl__trace__log (const char * msg,
                                          int          level,
                                          const char * section);

        void log (const string & msg,
                  log_levels     level,
                  const string & section)
        {
            agpl__trace__log (msg.c_str(), level, section.c_str());
        };

        void log (const char   * msg,
                  log_levels     level,
                  const string & section)
        {
            agpl__trace__log (msg, level, section.c_str());
        };

    }

}
