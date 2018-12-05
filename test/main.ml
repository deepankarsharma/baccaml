let arr = Array.make 10 0 in
let arr2 = Array.make 10 0 in
arr.(0) <- 1;
let res = interp arr2 0 arr 0 in
print_int res
