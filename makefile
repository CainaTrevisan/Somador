VHD=components.vhd somador.vhd

.phony: clean all

all: somador.vcd

somador.vcd: somador_tb
	./somador_tb --stop-time=25540us --vcd=somador.vcd

somador_tb: somador_tb.o
	ghdl -e --ieee=synopsys -fexplicit somador_tb

somador_tb.o: somador.o components.o
	ghdl -a --ieee=synopsys -fexplicit somador_tb.vhd

components.o:
	ghdl -a --ieee=synopsys -fexplicit components.vhd

somador.o:
	ghdl -a --ieee=synopsys -fexplicit somador.vhd

rand:
	g++ gera_in.cpp -o gera_in -m32 -std=c++11
	./gera_in > in

wave:
	gtkwave somador.vcd -a somador.sav &>/dev/null &

clean:
	$(RM) *.o .*.sw* *~
