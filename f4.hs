f4 n = if ((pi < 2*n) && (2*n < 2*pi)) || ((pi < 3*n) && (3*n < 2*pi)) then error "ERROR" else sqrt(sin(2*n))-sqrt(sin(3*n))
