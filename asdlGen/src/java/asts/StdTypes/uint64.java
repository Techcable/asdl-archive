/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_import=asts.StdPrims
  --line_width=74
  --no_action=false
  --output_directory=../asdl/tests
  --view=Java
  --xml_pickler=false
  */
package asts.StdTypes;
import asts.StdPrims.*;
public final class uint64 { public int int1;
    
    public uint64(int int1) {
        this.int1 = int1;
        
    }
    public final static void write(java.math.BigInteger x, 
				   java.io.OutputStream s) {
         g.write_java_BigInteger(x, s);
     }
    public final static java.math.BigInteger read(java.io.InputStream s) {
         java.math.BigInteger t1;
         t1 = g.read_java_BigInteger(s);
         return t1;
     }
    public final void accept(asts.StdTypes.Visitor v) {
         v.visit_uint64(this);
     }
}
