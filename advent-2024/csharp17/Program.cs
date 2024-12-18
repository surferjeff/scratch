using System;

class Machine {
    long A;
    long B;
    long C;

    int combo(int operand) {
        switch (operand) {
            case 0: case 1: case 2: case 3:
                return operand;
            case 4:
                return (int) A;
            case 5:
                return (int) B;
            case 6:
                return (int) C;
            default:
                throw new Exception("Bad combo operand " + operand);
        }
    }

    void op0(int operand) => A = A / (1L << combo(operand));
    void op1(int operand) => B = B ^ operand;
}