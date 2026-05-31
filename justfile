doc:
    R -q -e "devtools::document()"
check:
    R -q -e "devtools::check()"
test:
    R -q -e "devtools::test()"
