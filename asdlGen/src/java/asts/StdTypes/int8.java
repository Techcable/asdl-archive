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
public final class int8 { public int int1;
    
    public int8(int int1) {
        this.int1 = int1;
        
    }
    public final static void write(byte x, java.io.OutputStream s) {
         g.write_java_byte(x, s);
     }
    public final static byte read(java.io.InputStream s) {
         byte t1;
         t1 = g.read_java_byte(s);
         return t1;
     }
    public final void accept(asts.StdTypes.Visitor v) {
         v.visit_int8(this);
     }
}
