-record(fb_server, {id             ::integer(),
                    node           ::atom(),
                    pid            ::pid(),
                    total_size = 0 ::number()
                   }).
