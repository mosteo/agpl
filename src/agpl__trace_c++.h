#ifndef _agpl__trace_
#define _agpl__trace_

#include <string>

using namespace std;

namespace agpl {

    namespace trace {

        enum log_levels
               {NEVER       = 0,
                DEBUG       = 1,
                INFORMATIVE = 2,
                WARNING     = 3,
                ERROR       = 4,
                ALWAYS      = 5};

        void log (const string & msg,
                  log_levels     level,
                  const string & section);

        void log (const char   * msg,
                  log_levels     level,
                  const string & section);

    }

}

#endif
