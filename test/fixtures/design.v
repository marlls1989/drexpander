module design (clk, reset, a, b, y, z);
input  clk, reset;
input  a;
input  [1:0] b;
output y;
output [1:0] z;
wire   w;
assign w = a;
dff r0 (.q(y), .d(w));
tielo t0 (.y(z[0]));
endmodule
