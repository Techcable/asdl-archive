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
public final class bool { public final static int TRUE_enum = 0;
    public final static int FALSE_enum = 1;
     private int _kind;
    
    public bool(int _kind) {
        this._kind = _kind;
        
    }
    public final static void write(boolean x, java.io.OutputStream s) {
         g.write_java_boolean(x, s);
     }
    public final static boolean read(java.io.InputStream s) {
         boolean t1;
         t1 = g.read_java_boolean(s);
         return t1;
     }
    public final int kind() {
         return _kind;
     }
    public final void accept(asts.StdTypes.Visitor v) {
         v.visit_bool(this);
     }
}
