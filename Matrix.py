##
## General Matrix class
##
##

# =============================================================================
#   Note this class is primarily aimed for use of square matrices
# =============================================================================

class Matrix(object):
    
# =============================================================================
#     Matrix Class initialiser requires only dimensions, and also allows for 
#     optional setting of on of the standard matrices as described below.
# =============================================================================
    
    def __init__(self, m, n, Type="Zero"):
        
        # Standard Matrix types include
        # Zero Matrix = "Zero"
        # Identity Matrix = "Identity"
        # Elementary Matrices
        #
        # Add Scalar multiple of j'th row to i'th row = E1[scalar][i][j]
        # Swap i'th and j'th row                      = E2[i][j]
        # Multiply i'th row by scalar                 = E3[scalar][i]
        
        error_code = -1
        
        self._M = m
        self._N = n
        self._Contents = []
        self._Square = False
        self._RowReducedForm = 0
        self._ElementaryConstruct = []
        self._Determinant = "null"
        self._Inverse = 0
        
        if m == n:
            self._Square = True
        
        check = False
        
        if (m < 1 or n < 1):
            error_code = 0          # Dimensions too small
            check = True
        else:
            for j in range(1, self._N + 1):       # Initialise as Zero Matrix
                column = []
                for i in range(1, self._M + 1):
                    column.append(0)
                self._Contents.append(column)
        
        
        while not check:
                    
            if Type == "Zero":
                
                check = True
                
            elif not self._Square:
                error_code = 1      # Not Square
                Type = "Zero"
                    
            elif Type == "Identity":
                for i in range(1, m + 1):
                    for j in range(1, n + 1):
                        self.setEntry(i, j, 0)
                        
                for i in range(1, n + 1):
                    self.setEntry(i, i, 1)
                    check = True
                        
            elif Type[0] == 'E':
                for i in range(1, n + 1):       # Set to identity
                    self.setEntry(i, i, 1)
                    check = True
                k = 1
                if Type[k] == '1':
                    k += 1
                    if Type[k] == '[':
                        try:
                            number, k = self._getInnerBracket(Type, k)
                            scalar = float(number)
                            number, k = self._getInnerBracket(Type, k)        # Type[k] == '['
                            i = int(number)
                            number, k = self._getInnerBracket(Type, k)
                            j = int(number)
                            
                            self.setEntry(i, j, scalar)
                            check = True
                            
                        except:
                            error_code = 2  # Invalid entry for type E1
                            Type = "Identity"
                        
                    else:
                        error_code = 2
                        Type = "Identity"
                        
                elif Type[k] == '2':
                    k += 1
                    if Type[k] == '[':
                        try:
                            number, k = self._getInnerBracket(Type, k)
                            i = int(number)
                            number, k = self._getInnerBracket(Type, k)
                            j = int(number)
                            
                            self.setEntry(i, i, 0)
                            self.setEntry(j, j, 0)
                            self.setEntry(i, j, 1)
                            self.setEntry(j, i, 1)
                            
                            check = True
                        
                        except:
                            error_code = 3  # Invalid entry for type E2
                            Type = "Identity"
                            
                    else:
                        error_code = 3
                        Type = "Identity"
                        
                elif Type[k] == '3':
                    k += 1
                    if Type[k] == '[':
                        try:
                            number, k = self._getInnerBracket(Type, k)
                            scalar = float(number)
                            number, k = self._getInnerBracket(Type, k)
                            i = int(number)
                            
                            self.setEntry(i, i, scalar)
                            
                            check = True
                            
                        except:
                            error_code = 4  # Invalid entry for type E3
                            Type = "Identity"
                    
                    else:
                        error_code = 4
                        Type = "Identity"
                    
                else:
                    error_code = 5  # Invalid Type
                    Type = "Identity"
                    
                    
            else:
                error_code = 5
                Type = "Identity"
                
        
        if error_code == 0:
            print("ERROR - Matrix.__init__ - Dimesions too small for matrix")
        elif error_code == 1:
            print("ERROR - Matrix.__init__ - Matrix not square, defaulting to zero matrix")
        elif error_code == 2:
            print("ERROR - Matrix.__init__ - Invalid arguments for matrix type E1, defaulting to Identity Matrix")
        elif error_code == 3:
            print("ERROR - Matrix.__init__ - Invalid arguments for matrix type E2, defaulting to Identity Matrix")
        elif error_code == 4:
            print("ERROR - Matrix.__init__ - Invalid arguments for matrix type E3, defaulting to Identity Matrix")
        elif error_code == 5:
            print("ERROR - Matrix.__init__ - Invalid matrix type, defaulting to Identity Matrix")
                            
    
# =============================================================================
#   Returns Matrix dimension in a string format
# =============================================================================
    def getDim_Str(self):
        
        return (str(self._M) + ' x ' + str(self._N))
    
# =============================================================================
#   Returns Matrix Dimension's; an m x n matrix would return m, n
# =============================================================================
    def getDim(self):
        
        return self._M, self._N
    
# =============================================================================
#   Set's Matrix entries according to another matrix
# =============================================================================
    
    def setMatrix(self, A):
        error_code = -1
        A_M, A_N = A.getDim()
        
        if (self._M != A_M and self._N != A_N):
            error_code = 0                      # Invalid Dimensions
        else:
            for i in range(1, A_M + 1):
                for j in range(1, A_N + 1):
                    self.setEntry(i, j, A.getEntry(i, j))
        
        if error_code == 0:
            print("ERROR - Matrix.setMatrix - Matrix Dimensions do not match")
            
# =============================================================================
#   Set's matrix entries using a list of vectors, each vector will be appended
#   to the matrix's column
# =============================================================================
    def setMatrix_Vectors(self, Vectors):
        
        self._Contents = []
        
        error_code = -1
        vector_no = 0
        
        if len(Vectors) == self._N:
            for vector in Vectors:
                vector_no += 1
                if len(vector) == self._M:
                    self._Contents.append(vector)
                else:
                    error_code = 1  # Wrong vector dimension
                    break
        else:
            error_code = 0  # Not enough vectors
            
        if error_code == -1:
            print("Successfully appended vectors to matrix.")
        elif error_code == 0:
            print("ERROR - Matrix.setMatrix_Vectors - Not enough vectors passed through to method")
        elif error_code == 1:
            print("ERROR - Matrix.setMatrix_Vectors - Incorrect Vector dimension passed, Vector number " + str(vector_no))
            
        if error_code != -1:
            self._Contents = []
            
                
# =============================================================================
#   Returns the (i, j)th entry     
# =============================================================================
            
    def getEntry(self, i, j):
        return self._Contents[j-1][i-1]
    
# =============================================================================
#   Set's the (i, j)th entry
# =============================================================================
    
    def setEntry(self, i, j, entry):
        self._Contents[j-1][i-1] = entry
        
# =============================================================================
#   Returns the matrix transposed
# =============================================================================
    
    def Transpose(self):    # Returns Transpose of Matrix
        
        Result = Matrix(self._M, self._N)
        
        for i in range(self._M):
            for j in range(self._N):
                Result.setEntry(j, i, self.getEntry(i, j))
                
        return Result

# =============================================================================
#   Returns the Trace of the matrix
# =============================================================================
    
    def Trace(self):
        if not self._Square:
            print("ERROR - Matrix.Trace - Matrix is not square")
            return 0
        else:
            trace = 0
            for i in range(1, self._N + 1):
                trace += self.getEntry(i, j)
            return trace
    
# =============================================================================
#   Returns the resulting matrix after adding the matrix passed through the parameter
# =============================================================================
    def Add(self, A):
        
        A_M, A_N = A.getDim()
        if (A_M != self._M and A_N != self._N):
            print("ERROR - Matrix.Add - Invalid Matrix Dimensions")
            return Matrix(self._M, self._N)
        else:
            Result = self
            for i in range(1, A_M + 1):
                for j in range(1, A_N + 1):
                    Result.setEntry(i, j, Result.getEntry(i, j) + A.getEntry(i, j))
            
            return Result
# =============================================================================
#   Similar to add, except subtract
# =============================================================================
    def Subtract(self, A):
        
        return self.Add(A.Multiply_Scalar(-1))
    
# =============================================================================
#   Standard Matrix multiplication on the right
# =============================================================================
    
    def Multiply(self, A):
        
        error_code = -1
        
        A_M, A_N = A.getDim()
        
        if self._N == A_M:
            
            Result = Matrix(self._M, A_N)
            
            m, n = Result.getDim()
            
            for j in range(1, n + 1):
                for i in range(1, m + 1):
                    
                    result_entry = 0
                    
                    for k in range(1, self._N + 1):
                        result_entry += self.getEntry(i, k)*A.getEntry(k, j)
                    
                    Result.setEntry(i, j, result_entry)
        
        else:
            error_code = 0
            
        if error_code == -1:
            print("Successfully completed matrix multiplication")
        elif error_code == 0:
            print("ERROR - Matrix.Multiply - Invalid matrix dimensions for multiplication")
            
        return Result

# =============================================================================
#   Standard Matrix multiplication on the left
# =============================================================================
    
    def Multiply_L(self, A):
        
        return A.Multiply(self)
    
# =============================================================================
#   Multiply matrix by a scalar
# =============================================================================
    
    def Multiply_Scalar(self, scalar):
        
        Result = Matrix(self._M, self._N)
        
        for i in range(1, self._M):
            for j in range(1, self._N):
                Result.setEntry(i, j, self.getEntry(i, j)*scalar)
                
        return Result
    
# =============================================================================
#   Returns a copy of the matrix
# =============================================================================
    
    def Copy(self):
        
        return self
    
# =============================================================================
#   Returns the Matrix in a string format
# =============================================================================
    
    def toString(self):
        
        Result = ""
        for i in range(1, self._M + 1):
            row = "|  "
            for j in range(1, self._N + 1):
                row += (str(self.getEntry(i, j)) + "  ")
            row += "|"
            Result += (row + "\n")
        
        return Result
    
# =============================================================================
#   Returns the Row Reduced form of the matrix
# =============================================================================
        
    def RowReduce(self):        # Badman method, algorithm from proof
                                # MA106 Theorem 3.4
        try:
            self._RowReducedForm += 1
            i = 1
            j = 1
            Result = self      # check this works if we replace with just 'self'
            m, n = Result.getDim()
            #print(Result.toString())
            
            while (i <= m or j <= n):
                step1 = True
                while step1 and j <= n:
                    #print("( " + str(i) + " , " + str(j) + " )")
                    for k in range(i, m + 1):
                        if Result.getEntry(k, j) != 0:
                            step1 = False
                    if step1:
                        j += 1
                        
                if j <= n:
                    if Result.getEntry(i, j) == 0:
                        for k in range(i + 1, m + 1):
                            if Result.getEntry(k, j) != 0:
                                type_code = "E2[" + str(i) + "][" + str(k) + "]"
                                R2 = Matrix(n, n, Type=type_code)
                                Result = Result.Multiply_L(R2)
                                self._ElementaryConstruct.append(type_code)
                                #print(Result.toString())
                                
                    
                    if Result.getEntry(i, j) != 1:
                        scalar = 1 / Result.getEntry(i, j)
                        type_code = "E3[" + str(scalar) + "][" + str(i) + "]"
                        R3 = Matrix(n, n, Type=type_code)
                        Result = Result.Multiply_L(R3)
                        self._ElementaryConstruct.append(type_code)
                        #print(Result.toString())
                        
                        
                    for k in range(1, m + 1):
                        if k != i and Result.getEntry(k, j) != 0:
                            scalar = -1*Result.getEntry(k, j)
                            type_code = "E1[" + str(scalar) + "][" + str(k) + "][" + str(i) + "]"
                            R1 = Matrix(n, n, Type=type_code)
                            #print(R1.toString())
                            Result = Result.Multiply_L(R1)
                            self._ElementaryConstruct.append(type_code)
                            #print(Result.toString())
                            
                            
                i += 1
                j += 1
            
            self._RowReducedForm = Result
            return Result
        
        except:
            return self._RowReducedForm
        
# =============================================================================
#   Returns a boolean value confirming if the two matrices in question are equal
# =============================================================================
        
    def isEqual(self, A):
        A_M, A_N = A.getDim()
        
        if (self._M != A_M and self._N != A_N):
            return False
        else:
            for i in range(1, A_M + 1):
                for j in range(1, A_N + 1):
                    if self.getEntry(i, j) != A.getEntry(i, j):
                        return False
            
            return True
        
# =============================================================================
#   Returns the determinant of the matrix
# =============================================================================
        
    def Determinant(self):
        
        error_code = -1
        
        try:
            float(self._Determinant)
            return self._Determinant
        except:
            det = 1
            if not self._Square:
                type_code = 0
                det = 0
            else:
                Identity = Matrix(self._N, self._N, Type="Identity")
                
                if not Identity.isEqual(self.getRowReduce()):
                    det = 0
                else:
                    for type_code in self._ElementaryConstruct:
                        type_code = self._Inverse_Elementary(type_code)
                        det = det*self._det_Elementary(type_code)
                        
            if error_code == -1:
                print("Successfully Calculated Determinant")
            elif error_code == 0:
                print("ERROR - Matrix.getDeterminant - Matrix is not square")
            
            self._Determinant = det
            return det
        
# =============================================================================
#   Returns the Inverse of the matrix
# =============================================================================
        
    def Inverse(self):
        
        error_code = -1
        try:
            self._Inverse += 1
            Result = Matrix(self._N, self._N, Type=self._ElementaryConstruct[0])
            
            if self.getDeterminant() == 0:
                Result = Matrix(self._N, self._N, Type="Zero")
                error_code = 0
            else:
                for i in range(1, len(self._ElementaryConstruct)):
                    mtrx = Matrix(self._N, self._N, Type=self._ElementaryConstruct[i])
                    Result = Result.Multiply_L(mtrx)
                    
                self._Inverse = Result
        except:
            pass
        
        if error_code == -1:
            print("Successfully Calculated Determinant")
        elif error_code == 0:
            print("ERROR - Matrix.getInverse - Zero Determinant, Matrix is non invertable")
            
        return self._Inverse
    
# =============================================================================
#   Computes Row Rank and Row Nullity of the matrix
# =============================================================================

    def Nullity(self):
        
        RRF = self.RowReduce()
        Nullity = 0
        for i in range(1, self._M + 1):
            row_zero = True
            for j in range(1, self_N + 1):
                if RRF.getEntry(i, j) != 0:
                    row_zero = False
            
            if row_zero:
                Nullity += 1
        
        return Nullity
    
    def Rank(self):
        
        return (self._M - self.Nullity())
    
# =============================================================================
#   Private Methods to aid in modularising the code
# =============================================================================
                
        
    def _Inverse_Elementary(self, type_code):
        
        if type_code[1] == '1':
            k = 2
            number, k = self._getInnerBracket(type_code, k)
            scalar = -1*float(number)
            type_code = "E1[" + str(scalar) + "]" + type_code[k:]
        
        elif type_code[1] == '2':
            pass
        
        elif type_code[1] == '3':
            k = 2
            number, k = self._getInnerBracket(type_code, k)
            scalar = 1/float(number)
            type_code = "E3[" + str(scalar) + "]" + type_code[k:]
        
        else:
            print("ERROR - Matrix._Inverse_Elementary - Invalid type code")
            
        return type_code
                
    def _det_Elementary(self, type_code):
        
        det = 0
        
        if type_code[1] == '1':
            det = 1
        elif type_code[1] == '2':
            det = -1
        elif type_code[1] == '3':
            k = 2
            number, k = self._getInnerBracket(type_code, k)
            det = float(number)
        
        return det
            
            
    def _getInnerBracket(self, code, k):
        number = ""                     # code[k] = '['
        k += 1
        char = code[k]      
        while char != ']':
            number += char
            k += 1
            char = code[k]
        
        k += 1
        
        return number, k
                        
    
    





       
                        
                    
        
        
            

    
    
    
                

    
    
    
    
        
            
    
    
    
    