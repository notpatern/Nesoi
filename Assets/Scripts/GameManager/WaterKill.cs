using UnityEngine;

public class WaterKill : MonoBehaviour
{
    private void OnTriggerEnter(Collider other)
    {
        if (!other.CompareTag("Player")) return;
        other.GetComponent<IDamageable>().TakeDamage(86876878);
    }
}
