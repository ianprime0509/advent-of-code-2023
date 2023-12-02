import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Part1 {
  public static void main(String[] args) throws IOException {
    var idSum = 0;
    try (var br = Files.newBufferedReader(Path.of("input.txt"));
        var scanner = new Scanner(br)) {
      while (scanner.hasNextLine()) {
        var game = Game.parse(scanner.nextLine());
        if (game.isPossible()) {
          idSum += game.id();
        }
      }
    }
    System.out.println(idSum);
  }

  public record Game(int id, List<Draw> draws) {
    public static Game parse(String s) {
      var colonPos = s.indexOf(':');
      var id = Integer.parseInt(s.substring("Game ".length(), colonPos));
      var draws = new ArrayList<Draw>();
      for (var draw : s.substring(colonPos + ": ".length()).split("; ")) {
        draws.add(Draw.parse(draw));
      }
      return new Game(id, draws);
    }

    public boolean isPossible() {
      for (var draw : draws) {
        if (draw.red() > 12 || draw.green() > 13 || draw.blue() > 14) {
          return false;
        }
      }
      return true;
    }

    public record Draw(int red, int green, int blue) {
      public static Draw parse(String s) {
        var red = 0;
        var green = 0;
        var blue = 0;
        for (var pick : s.split(", ")) {
          var pickParts = pick.split(" ");
          var count = Integer.parseInt(pickParts[0]);
          switch (pickParts[1]) {
            case "red" -> red = count;
            case "green" -> green = count;
            case "blue" -> blue = count;
          }
        }
        return new Draw(red, green, blue);
      }
    }
  }
}
