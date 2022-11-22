#ifndef RUBY_IPL_HTML_WRAPPERS_HPP_
#define RUBY_IPL_HTML_WRAPPERS_HPP_

#define VIEW_WRAPPER_BEGIN "<div style=\""             \
                           " display: flex;"           \
                           " width: 100%;"             \
                           " height: 100%;"            \
                           " justify-content: center;" \
                           " align-items: center;"     \
                           " font-size: 3.5rem;"       \
                           "\"><pre style=\""          \
                           " width: 100%;"             \
                           " height: 100%;"            \
                           "\">"

#define VIEW_WRAPPER_END "</pre></div>"

#define BOOTSTRAP_HOVER_INFORMATION "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\">" \
                                    "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>"               \
                                    "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\"></script>"            \
                                    "<script>"                                                                                                 \
                                    "  $(document).ready(() => {"                                                                              \
                                    "    $('.token').popover({"                                                                                \
                                    "      trigger: 'hover',"                                                                                  \
                                    "      placement: 'right'"                                                                                 \
                                    "    });"                                                                                                  \
                                    "  });"                                                                                                    \
                                    "</script>"

#endif
