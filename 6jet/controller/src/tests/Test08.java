package tests;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.ByteArrayInputStream;

import javax.swing.JFrame;
import javax.swing.border.LineBorder;

import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.common.ui.JetDisplayComponent;
import org.opengroove.sixjet.common.ui.JetDisplayListener;

public class Test08 {
	public static byte[] testDescriptor = ("440 440\r\n" + "jet 0 255 255\r\n"
			+ "jet 1 255 185\r\n" + "jet 2 185 185\r\n" + "jet 3 185 255\r\n"
			+ "jet 4 323 279\r\n" + "jet 5 323 160\r\n" + "jet 6 220 101\r\n"
			+ "jet 7 117 160\r\n" + "jet 8 117 279\r\n" + "jet 9 220 339\r\n"
			+ "jet 10 315 385\r\n" + "jet 11 410 220\r\n" + "jet 12 315 55\r\n"
			+ "jet 13 125 55\r\n" + "jet 14 30 220\r\n" + "jet 15 125 385")
			.getBytes();

	public static void main(String[] args) throws Throwable {
		DescriptorFile file = new DescriptorFile(new ByteArrayInputStream(
				testDescriptor));
		final JetDisplayComponent display = new JetDisplayComponent(file);
		display.addJetListener(new JetDisplayListener() {

			public void jetDown(int jet) {
				display.setState(jet, true);
			}

			public void jetUp(int jet, boolean in) {
				display.setState(jet, false);
			}
		});
		JFrame f = new JFrame();
		f.setSize(600, 600);
		f.getContentPane().setLayout(new BorderLayout());
		f.setLocationRelativeTo(null);
		f.getContentPane().add(display);
		f.show();

	}
}
