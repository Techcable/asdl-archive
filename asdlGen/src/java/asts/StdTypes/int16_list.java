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
public final class int16_list {
    public short head;
    public int16_list tail;
    
    public int16_list(short head, int16_list tail)
    {
        this.head = head;
        this.tail = tail;
    }
    public final void accept(Visitor v)
     {
         v.visit_int16_list(this);
     }
    public final static int16_list read(java.io.InputStream s_)
     {
         int16_list ret_;
         {
             int t_1;
             int16_list t_2;
             t_1 = g.read_tag(s_);
             if(t_1 != 0)
                 ret_ = new int16_list(int16.read(s_), null);
              else
                 return null;
             t_1 = t_1 - 1;
             t_2 = ret_;
             while(t_1 != 0)
             {
                 t_2.tail = new int16_list(int16.read(s_), null);
                 t_2 = t_2.tail;
                 t_1 = t_1 - 1;
             }
         }
         return ret_;
     }
    public final static void write(int16_list x_, java.io.OutputStream s_)
     {
         {
             int t_1;
             int16_list t_2;
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
                 int16.write(t_2.head, s_);
                 t_2 = t_2.tail;
                 t_1 = t_1 - 1;
             }
         }
     }
}