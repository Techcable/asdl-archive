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
public final class ieee_real { public String string1;
    
    public ieee_real(String string1) {
        this.string1 = string1;
        
    }
    public final static void write(double x, java.io.OutputStream s) {
         g.write_java_double(x, s);
     }
    public final static double read(java.io.InputStream s) {
         double t1;
         t1 = g.read_java_double(s);
         return t1;
     }
    public final void accept(asts.StdTypes.Visitor v) {
         v.visit_ieee_real(this);
     }
}
