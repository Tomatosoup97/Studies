./xi --plugin xisdk/mod_uwr.cma ./tests/pracownia1/sandbox.xi --stop-after parser
cat xilog/002.parsing.raw.ast > uwr_tree

./xi --plugin mods/mod_student.cma ./tests/pracownia1/sandbox.xi --stop-after parser
cat xilog/002.parsing.raw.ast > student_tree

diff uwr_tree student_tree

