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
public final class nat { public int int1;
    
    public nat(int int1) {
        this.int1 = int1;
        
    }
    public final static void write(asts.StdTypes.nat x, java.io.OutputStream s) {
         g.write_int(x.int1, s);
     }
    public final static asts.StdTypes.nat read(java.io.InputStream s) {
         asts.StdTypes.nat t1;
         int int1;
         int1 = g.read_int(s);
         t1 = new asts.StdTypes.nat(int1);
         return t1;
     }
    public final void accept(asts.StdTypes.Visitor v) {
         v.visit_nat(this);
     }
}
