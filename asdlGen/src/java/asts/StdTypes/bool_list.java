/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_import asdl_base
  --line_width 74
  --no_action false
  --output_directory ./
  --view Java
  */
package asts.StdTypes;
import asdl_base.*;
public final class bool_list {
    public boolean head;
    public bool_list tail;
    
    public bool_list(boolean head, bool_list tail)
    {
        this.head = head;
        this.tail = tail;
    }
    public final void accept(Visitor v)
     {
         v.visit_bool_list(this);
     }
    public final static bool_list read(java.io.InputStream s_)
     {
         bool_list ret_;
         {
             int t_1;
             bool_list t_2;
             t_1 = g.read_tag(s_);
             if(t_1 != 0)
                 ret_ = new bool_list(bool.read(s_), null);
              else
                 return null;
             t_1 = t_1 - 1;
             t_2 = ret_;
             while(t_1 != 0)
             {
                 t_2.tail = new bool_list(bool.read(s_), null);
                 t_2 = t_2.tail;
                 t_1 = t_1 - 1;
             }
         }
         return ret_;
     }
    public final static void write(bool_list x_, java.io.OutputStream s_)
     {
         {
             int t_1;
             bool_list t_2;
             t_1 = 0;
             t_2 = x_;
             while(t_2 != null)
             {
                 t_2 = t_2.tail;
                 t_1 = t_1 + 1;
             }
             g.write_tag(t_1, s_);
             t_2 = x_;
             while(t_1 != 0)
             {
                 bool.write(t_2.head, s_);
                 t_2 = t_2.tail;
                 t_1 = t_1 - 1;
             }
         }
     }
}