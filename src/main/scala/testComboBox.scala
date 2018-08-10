import swing._
import event._
import ComboBox._

object testComboBox extends SimpleSwingApplication {
    val ui = new FlowPanel {
        val box = new ComboBox(List(1,2,3,4))
        reactions += {
            case SelectionChanged(`box`) =>
                Console.out.println("selection changed")
        }
    }

    def top = new MainFrame {
        contents = ui
    }
}

